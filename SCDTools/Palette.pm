###############################################################################
# Copyright (c) 2011 by bgvanbur
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2 of the License, or (at your
# option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
###############################################################################
# handle palettes for megadrive/megacd development
###############################################################################

package SCDTools::Palette;
use strict;
use warnings;

my $tmpFile = 'TMP.BIN';
my $tmpFileRemoveAfterUse = 1;

sub GetHelpString {
    return '[options-palette]
  -asmpal=<file>        output palette to assembly file
  -binpal=<file>        output palette to binary file
  -palfile=<file>       specifies the palette
  -paloffset=<#>        palette offset of the first palette in the palette file
  -palbyteoffset=<#>    byte offset of the first palette in the palette file
  -magicpink            treat #FF00FF as transparent
  -magicblack           treat #000000 as transparent
';
}

sub new {
   my($class) = @_;
 
   my $self = bless({}, $class);
 
   $self->{'colors'} = [];
   $self->{'colorsCount'} = 0;
   $self->{'colorsMissing'} = {};
   $self->{'paletteCount'} = 1;
   $self->{'paletteFileOutput'} = '';
   $self->{'paletteFileOutputBin'} = 0;
   $self->{'paletteFileInput'} = '';
   $self->{'paletteFileInputOffset'} = 0;
   $self->{'magicPink'} = 0;
   $self->{'magicBlack'} = 0;
   $self->{'verbosity'} = 2;
   $self->{'comments'} = 1;
   $self->{'labels'} = 1;
   $self->{'label'} = '';
 
   return $self;
}

sub SetVerbosity {
    my ($self,$verbosity) = @_;
    $self->{'verbosity'} = $verbosity;
}

sub SetComments {
    my ($self,$comments) = @_;
    $self->{'comments'} = $comments;
}

sub SetLabels {
    my ($self,$labels) = @_;
    $self->{'labels'} = $labels;
}

sub SetLabel {
    my ($self,$label) = @_;
    $self->{'label'} = $label;
}

sub ParseArg {
    my ($self,$arg) = @_;
    if ( $arg =~ /^-pal(ette)?count=(\d+)$/i ) {
 	$self->{'paletteCount'} = $2;
    } elsif ( $arg =~ /^-asmpal(ette)?=(.*)$/i ) {
 	$self->{'paletteFileOutput'} = $2;
 	$self->{'paletteFileOutputBin'} = 0;
    } elsif ( $arg =~ /^-binpal(ette)?=(.*)$/i ) {
 	$self->{'paletteFileOutput'} = $2;
 	$self->{'paletteFileOutputBin'} = 1;
    } elsif ( $arg =~ /^-pal(ette)?file=(.+)$/i ) {
 	$self->{'paletteFileInput'} = $2;
    } elsif ( $arg =~ /^-pal(ette)?byteoffset=(\d+)/i ) {
 	$self->{'paletteFileInputOffset'} = $2;
    } elsif ( $arg =~ /^-pal(ette)?offset=(\d+)$/i ) {
 	$self->{'paletteFileInputOffset'} = 32 * $2;
    } elsif ( $arg =~ /^-magicpink$/i ) {
	$self->{'magicPink'} = 1;
    } elsif ( $arg =~ /^-magicblack$/i ) {
	$self->{'magicBlack'} = 1;
    } else {
	if ( $arg =~ /^-nocomments$/i ) {
	    $self->{'comments'} = 0;
	} elsif ( $arg =~ /^-nolabels$/i ) {
	    $self->{'labels'} = 0;
	} elsif ( $arg =~ /^-label=(.+)$/i ) {
	    $self->{'label'} = $1;
	} elsif ( $arg =~ /^-v=(\d+)$/i ) {
	    $self->{'verbosity'} = $1;
	}
	return 0;
    }
    return 1;
}

sub ReadPalette {
    my ($self) = @_;
    my $paletteFileInput = $self->{'paletteFileInput'};
    if ( $paletteFileInput eq '' ) {
	return;
    }
    if ( ! -e $paletteFileInput ) {
	die "Bad palette file: ".$paletteFileInput."\n";
    }
    my $tmpFileRemove = 0;
    if ( $paletteFileInput =~ m/\.(asm|68k)$/i ) {
	my $paletteAsmFile = $paletteFileInput;
	$paletteFileInput = $tmpFile;
	$tmpFileRemove = $tmpFileRemoveAfterUse;
	system("scdasm -v=".$self->{'verbosity'}." $paletteAsmFile ".$paletteFileInput);
	if ( ! -e $paletteFileInput ) {
	    die "Bad scdasm since didn't make: $paletteFileInput";
	}
    }

    if ( $self->{'paletteCount'} <= 0 ) {
	$self->{'paletteCount'} = ( ( -s $paletteFileInput ) - $self->{'paletteFileInputOffset'} ) >> 5;
    }

    my $paletteCountInBytes = 32 * $self->{'paletteCount'};
    my $paletteData = chr(0x00)x$paletteCountInBytes;

    open( PALETTE, $paletteFileInput ) or die "Cannot read palette file: $!\n";
    binmode PALETTE;
    seek(PALETTE,$self->{'paletteFileInputOffset'},0);
    my $paletteDataBuffer;
    if ( read(PALETTE,$paletteDataBuffer,$paletteCountInBytes) == $paletteCountInBytes ) {
	$paletteData = $paletteDataBuffer;
	for ( my $paletteDataByte = 0; $paletteDataByte < $paletteCountInBytes;$paletteDataByte += 2 ) {
	    if ( $paletteDataByte % 32 > 0 ) {
		$self->AddColor(substr($paletteData,$paletteDataByte,2));
	    }
	}
    }
    close PALETTE;

    if ( $tmpFileRemove ) {
	unlink $tmpFile;
    }
}

sub OutputPalette {
    my ($self) = @_;
    if ( $self->{'paletteFileOutput'} eq '' ) {
	return;
    }
    my $fileContents = $self->GetOutputPaletteData($self->{'paletteFileOutputBin'});
    open(PALETTE,'>'.$self->{'paletteFileOutput'});
    if ( $self->{'paletteFileOutputBin'} ) {
	binmode PALETTE;
    }
    print PALETTE $fileContents;
    close PALETTE;
}

sub GetOutputPaletteData {
    my ($self,$bin) = @_;
    if ( $self->{'colorsCount'} <= 0 ) {
	return '';
    }
    my $fileContents = '';
    if ( ! $bin ) {
	if ( $self->{'comments'} ) {
	    $fileContents .= ";; palette\n";
	}
	if ( $self->{'labels'} ) {
	    $fileContents .= "Start".$self->{'label'}."Palette:\n";
	}
    }
    for ( my $paletteIndex = 0; $paletteIndex < $self->{'paletteCount'}; $paletteIndex++ ) {
 	if ( $bin ) {
 	    $fileContents .= pack("n",0x0000);
 	} else {   
	    $fileContents .= " dc.w 0x0000\n";
	}
	for ( my $i = 0; $i < 15; $i++ ) {
	    my $colorsIndex = $paletteIndex * 15 + $i;
	    if ( $colorsIndex < $self->{'colorsCount'} ) {
		if ( $bin ) {
		    $fileContents .= $self->{'colors'}[$colorsIndex];
		} else {
		    my $colorf = sprintf("0x%4.4X",unpack("n",$self->{'colors'}[$colorsIndex]));
		    $fileContents .= ' dc.w '.$colorf."\n";
		}
	    } else {
		if ( $bin ) {
		    $fileContents .= pack("n",0x0000);
		} else {
		    $fileContents .= " dc.w 0x0000\n";
		}
	    }
	}
    }
    if ( ! $bin && $self->{'labels'} ) {
	$fileContents .= "End".$self->{'label'}."Palette:\n";
    }
    return $fileContents;
}

sub GetColorIndex {
    my ($self,$color) = @_;
    my $index = -1;
    if ( ( $self->{'magicPink'} && $color eq (chr(0x0E).chr(0x0E)) ) ||
	 ( $self->{'magicBlack'} && $color eq (chr(0x00).chr(0x00)) ) ) {
	$index = 0;
    } else {
	$index = $self->GetColorIndexWithNoAdding($color);
	if ( $index < 0 ) {
	    $index = $self->AddColor($color);
	}
    }
    return $index;
}

sub GetColorIndexWithNoAdding {
    my ($self,$color) = @_;
    my $index = -1;
    my $colorsCount = $self->{'colorsCount'};
    for ( my $colorsIndex = 0; $colorsIndex < $colorsCount; $colorsIndex++ ) {
	if ( $color eq $self->{'colors'}[$colorsIndex] ) {
	    # account for each palette having unused transparent index
	    $index = $colorsIndex + int($colorsIndex / 15) + 1;
	    last;
	}
    }
    return $index;
}

sub AddColor {
    my ($self,$color) = @_;
    my $index = -1;
    if ( $self->{'colorsCount'} < 15 * $self->{'paletteCount'} ) {
	push @{$self->{'colors'}}, $color;
	$self->{'colorsCount'}++;
	$index = $self->{'colorsCount'} + int($self->{'colorsCount'} / 15);
    } else {
	$self->{'colorsMissing'}{$color}++;
	$index = 0;
    }
    return $index;
}

sub UpdateImageMagickPalette {
    my ($self,$imgPalette) = @_;
    use Image::Magick;

    my $colorsLength = $self->{'colorsLength'};

    #my $imgPalette = Image::Magick->new();
    $imgPalette->Set(size=>"${colorsLength}x1");
    my $x = $imgPalette->ReadImage('xc:black');
    warn $x if $x;

    for ( my $colorsIndex = 0; $colorsIndex < $colorsLength; $colorsIndex++ ) {
	my $c = $self->{'colors'}[$colorsIndex];
	my @pixels = ( (   ord(substr($c,0,1))        & 0xE ) / 14,
		       ( ( ord(substr($c,1,1)) >> 4 ) & 0xE ) / 14,
		       (   ord(substr($c,1,1))        & 0xE ) / 14 );
	for ( my $i = 0; $i < 3; $i++ ) {
	    $pixels[$i] = $pixels[$i]*0.95 + 0.025;
	}
	$imgPalette->SetPixel('channel'=>'RGB','x'=>$x,'y'=>0,'color'=>\@pixels);
    }
}

1;



# if ( $missingColors ) {
#     print STDERR "Colors detected exceeded colors allowed in palette\n";
# }

###############################################################################
# OLD CODE that might be useful to reinstate
###############################################################################

# my @colors = sort {$missingColors{$b} <=> $missingColors{$a}} keys %missingColors;

# if ( $paletteGet ) {
#     my $index = 0;
#     if ( $#colors + 1 > $paletteCount * 16 - 1 ) {
# 	print STDERR "Too many colors for one palette (will use most frequent colors)\n";
#     }
#     for ( my $paletteIndex = 0; $paletteIndex < $paletteCount; $paletteIndex++ ) {
# 	if ( $bin ) {
# 	    print pack("n",0x0000);
# 	} else {   
# 	    if ( $comments ) {
# 		print ";; palette\n";
# 	    }
# 	    print " dc.w 0x0000\n";
# 	}
# 	for ( my $i = 0; $i < 15; $i++ ) {
# 	    my $colorsIndex = $paletteIndex * 15 + $i;
# 	    if ( $colorsIndex <= $#colors ) {
# 		if ( $bin ) {
# 		    print $colors[$colorsIndex];
# 		} else {
# 		    my $colorf = sprintf("0x%4.4X",unpack("n",$colors[$colorsIndex]));
# 		    print ' dc.w '.$colorf."\n";
# 		}
# 	    } else {
# 		if ( $bin ) {
# 		    print pack("n",0x0000);
# 		} else {
# 		    print " dc.w 0x0000\n";
# 		}
# 	    }
# 	}
#     }
# } else {
#     foreach my $color (@colors) {
# 	my $count = $missingColors{$color};
# 	my $colorf = sprintf("0x%4.4X",unpack("n",$color));
# 	print STDERR "Palette does not contain color: $colorf ($count)\n";
#     }
# }
