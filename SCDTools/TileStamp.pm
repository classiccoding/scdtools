###############################################################################
# Copyright (c) 2012 by bgvanbur
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
# handle tiles/stamps for megadrive/megacd development
###############################################################################

package SCDTools::TileStamp;
use strict;
use warnings;

my $tmpFile = 'TMP.BIN';
my $tmpFileRemoveAfterUse = 1;
my $constantColorBlack = chr(0x00).chr(0x00);
my $constantColorPink  = chr(0x0E).chr(0x0E);

sub GetHelpString {
    return '[options-palette]
  -asmtile=<file>       output palette to assembly file
  -bintile=<file>       output palette to binary file
  -tilefile=<file>      specifies the palette
  -tileoffset=<#>       palette offset of the first palette in the palette file
  -tilebyteoffset=<#>   byte offset of the first palette in the palette file
  -plane                specifies plane 8x8 tile order and no tile reuse
                        (default)
  -planewithmap         specifies plane 8x8 tile order and tile reuse
  -sprite               specifies sprite 8x8 tile order and no tile reuse
  -stamp16x16           specifies 16x16 stamps with stamp reuse
  -stamp32x32           specifies 32x32 stamps with stamp reuse
  -tilescolorbits=<#>   specifies bits per color of 1, 2, or 4 (default is 4)
  -tilestransforms      allows 8x8 tile horizontal flip and vertical flip
                        allows 16x16 or 32x32 stamp horizontal flip and rotation
                        (these require a 2 byte tile map)
';
}

sub ParseArg {
    my ($self,$arg) = @_;
    if ( $arg =~ /^-asmtiles=(.*)$/i ) {
 	$self->{'fileOutputAsm'} = $1;
    } elsif ( $arg =~ /^-bintiles=(.*)$/i ) {
 	$self->{'fileOutputBin'} = $1;
    } elsif ( $arg =~ /^-tilesfile=(.+)$/i ) {
 	$self->{'fileInput'} = $2;
    } elsif ( $arg =~ /^-tilesbyteoffset=(\d+)/i ) {
 	$self->{'fileInputOffset'} = $1;
    } elsif ( $arg =~ /^-tilesoffset=(\d+)$/i ) {
 	$self->{'fileInputOffset'} = $self->{'tilesLength'} * $1;
    } elsif ( $arg =~ /^-plane$/i ) {
	$self->SetSize(8,8);
	$self->{'isTile'} = 1;
	$self->{'tilesOrderPlane'} = 1;
	$self->{'tilesReuse'} = 0;
    } elsif ( $arg =~ /^-planewithmap$/i ) {
	$self->SetSize(8,8);
	$self->{'isTile'} = 1;
	$self->{'tilesWidth'} = 8;
	$self->{'tilesHeight'} = 8;
	$self->{'tilesOrderPlane'} = 1;
	$self->{'tilesReuse'} = 1;
    } elsif ( $arg =~ /^-sprite$/i ) {
	$self->SetSize(8,8);
	$self->{'isTile'} = 1;
	$self->{'tilesWidth'} = 8;
	$self->{'tilesHeight'} = 8;
	$self->{'tilesOrderPlane'} = 0;
	$self->{'tilesReuse'} = 0;
    } elsif ( $arg =~ /^-stamp16x16$/i ) {
	$self->SetSize(16,16);
	$self->{'isTile'} = 0;
	$self->{'tilesOrderPlane'} = 1;
	$self->{'tilesReuse'} = 1;
    } elsif ( $arg =~ /^-stamp32x32$/i ) {
	$self->SetSize(32,32);
	$self->{'isTile'} = 0;
	$self->{'tilesOrderPlane'} = 1;
	$self->{'tilesReuse'} = 1;
    } elsif ( $arg =~ /^-tilescolorbits=([124])$/i ) {
	$self->SetColorBits($1);
    } elsif ( $arg =~ /^-tilestransforms$/i ) {
	$self->{'tilesTransforms'} = 1;
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

sub new {
   my($class) = @_;
 
   my $self = bless({}, $class);
 
   $self->{'tiles'} = [];
   $self->{'tilesCount'} = 0;
   $self->{'tilesCountRead'} = 0;
   $self->{'isTile'} = 1;
   $self->{'tilesWidth'} = 8;
   $self->{'tilesHeight'} = 8;
   $self->{'tilesOrderPlane'} = 1;
   $self->{'tilesLength'} = 32;
   $self->{'tilesReuse'} = 0;
   $self->{'tilesColorBits'} = 4;
   $self->{'tilesTransforms'} = 0;
   $self->{'fileOutputAsm'} = '';
   $self->{'fileOutputBin'} = '';
   $self->{'fileInput'} = '';
   $self->{'fileInputOffset'} = 0;
   $self->{'verbosity'} = 2;
   $self->{'comments'} = 1;
   $self->{'labels'} = 1;
   $self->{'label'} = '';
 
   return $self;
}

sub SetSize {
    my ($self,$width,$height) = @_;
    $self->{'tilesWidth'} = $width;
    $self->{'tilesHeight'} = $height;
    $self->{'tilesLength'} = $self->{'tilesColorBits'} * $self->{'tilesWidth'} * $self->{'tilesHeight'} / 8;
}

sub SetColorBits {
    my ($self,$colorBits) = @_;
    $self->{'tilesBitPerColor'} = $colorBits;
    $self->{'tilesLength'} = $self->{'tilesColorBits'} * $self->{'tilesWidth'} * $self->{'tilesHeight'} / 8;
}






sub ReadPalette {
    my ($self) = @_;
    my $fileInput = $self->{'fileInput'};
    if ( $fileInput eq '' ) {
	return;
    }
    if ( ! -e $fileInput ) {
	die "Bad tiles file: ".$fileInput."\n";
    }
    my $tmpFileRemove = 0;
    if ( $fileInput =~ m/\.(asm|68k)$/i ) {
	my $asmFile = $fileInput;
	$fileInput = $tmpFile;
	$tmpFileRemove = $tmpFileRemoveAfterUse;
	system("scdasm -v=".$self->{'verbosity'}." $asmFile ".$fileInput);
	if ( ! -e $fileInput ) {
	    die "Bad scdasm since didn't make: $fileInput";
	}
    }

    if ( $self->{'paletteCount'} <= 0 ) {
	# allow partial palette to count as a whole palette
	$self->{'paletteCount'} = ( ( -s $fileInput ) - $self->{'fileInputOffset'} + 31 ) >> 5;
    }

    my $length = $self->{'tilesLength'};
    my $count = int( ( ( -s $fileInput ) + $length - 1 ) / $length ) * $length;

    my $paletteCountInBytes = 32 * $self->{'paletteCount'};
    my $paletteData = chr(0x00)x$paletteCountInBytes;

    open( DATA, $fileInput ) or die "Cannot read palette file: $!\n";
    binmode DATA;
    seek(DATA,$self->{'fileInputOffset'},0);
    my $paletteReadCountInBytes = read(DATA,$paletteData,$paletteCountInBytes);
    # for a partial palette, do not add empty colors
    # will add these on output
    for ( my $paletteDataByte = 0; $paletteDataByte < $paletteReadCountInBytes; $paletteDataByte += 2 ) {
	# may add color zero of each palette depending on -colorzeronormal
	if ( ( ( $paletteDataByte >> 1 ) & 0xF ) >= 16 - $self->{'colorsPerPalette'} ) {
	    $self->AddColor(substr($paletteData,$paletteDataByte,2));
	}
    }
    close DATA;

    if ( $tmpFileRemove ) {
	unlink $tmpFile;
    }

    $self->{'colorsCountRead'} = $self->{'colorsCount'};
}


sub ReadTiles {
    if ( $tilesFile eq '' ) {
	return;
    }
    if ( ! -e $tilesFile ) {
	die "Bad tiles file: $tilesFile";
    }
    my $tmpFileRemove = 0;
    if ( $tilesFile =~ m/\.(asm|68k)$/i ) {
	my $tilesAsmFile = $tilesFile;
	$tilesFile = $tmpFile;
	$tmpFileRemove = $tmpFileRemoveAfterUse;
	system("scdasm -v=$verbosity $tilesAsmFile $tilesFile");
	if ( ! -e $tilesFile ) {
	    die "Bad scdasm since didn't make: $tilesFile";
	}
    }

    if ( $tilesCount <= 0 ) {
	$tilesCount = ( ( -s $tilesFile ) - $tilesOffset ) >> $tilesWidthShift;
    }

    my $tilesCountInBytes = $tilesCount*$tilesWidth;
    my $tilesData = chr(0x00)x$tilesCountInBytes;

    open( TILES, $tilesFile ) or die "Cannot read tiles file: $!\n";
    binmode TILES;
    seek(TILES,$tilesOffset,0);
    my $tilesDataBuffer;
    if ( read(TILES,$tilesDataBuffer,$tilesCountInBytes) == $tilesCountInBytes ) {
	$tilesData = $tilesDataBuffer;
	for ( my $tilesIndex = 0; $tilesIndex < $tilesCount; $tilesIndex++ ) {
	    my $tileData = chr(0x00)x32;
	    if ( $tilesCompress == 0 ) {
		$tileData = substr($tilesData,$tilesIndex*$tilesWidth,$tilesWidth);
	    } else {
		# TODO
		die "Cannot handle unextracting compressed tiles yet\n";
	    }
	    push @tiles, $tileData;
	}
    }
    close TILES;

    if ( $tmpFileRemove ) {
	unlink $tmpFile;
    }

    $self->{'colorsCountRead'} = $self->{'colorsCount'};
}



















# sub SetMaxColorCount {
#     my ($self,$count) = @_;
#     if ( $self->{'colorsCount'} > $count ) {
# 	splice(@{$self->{'colors'}},$count);
# 	$self->{'colorsCount'} = $count;
# 	if ( $count < $self->{'colorsCountRead'} ) {
# 	    $self->{'colorsCountRead'} = $count;
# 	}
#     }
# }

# sub GetColorsCountRead {
#     my ($self) = @_;
#     return $self->{'colorsCountRead'};
# }

# sub GetColorsCount {
#     my ($self) = @_;
#     return $self->{'colorsCount'};
# }

# sub GetColorsPerPalette {
#     my ($self) = @_;
#     return $self->{'colorsPerPalette'};
# }

# sub SetPaletteCount {
#     my ($self,$arg) = @_;
#     $self->{'paletteCount'} = $arg;
# }



# sub ReadPalette {
#     my ($self) = @_;
#     my $paletteFileInput = $self->{'paletteFileInput'};
#     if ( $paletteFileInput eq '' ) {
# 	return;
#     }
#     if ( ! -e $paletteFileInput ) {
# 	die "Bad palette file: ".$paletteFileInput."\n";
#     }
#     my $tmpFileRemove = 0;
#     if ( $paletteFileInput =~ m/\.(asm|68k)$/i ) {
# 	my $paletteAsmFile = $paletteFileInput;
# 	$paletteFileInput = $tmpFile;
# 	$tmpFileRemove = $tmpFileRemoveAfterUse;
# 	system("scdasm -v=".$self->{'verbosity'}." $paletteAsmFile ".$paletteFileInput);
# 	if ( ! -e $paletteFileInput ) {
# 	    die "Bad scdasm since didn't make: $paletteFileInput";
# 	}
#     }

#     if ( $self->{'paletteCount'} <= 0 ) {
# 	# allow partial palette to count as a whole palette
# 	$self->{'paletteCount'} = ( ( -s $paletteFileInput ) - $self->{'paletteFileInputOffset'} + 31 ) >> 5;
#     }

#     my $paletteCountInBytes = 32 * $self->{'paletteCount'};
#     my $paletteData = chr(0x00)x$paletteCountInBytes;

#     open( PALETTE, $paletteFileInput ) or die "Cannot read palette file: $!\n";
#     binmode PALETTE;
#     seek(PALETTE,$self->{'paletteFileInputOffset'},0);
#     my $paletteReadCountInBytes = read(PALETTE,$paletteData,$paletteCountInBytes);
#     # for a partial palette, do not add empty colors
#     # will add these on output
#     for ( my $paletteDataByte = 0; $paletteDataByte < $paletteReadCountInBytes; $paletteDataByte += 2 ) {
# 	# may add color zero of each palette depending on -colorzeronormal
# 	if ( ( ( $paletteDataByte >> 1 ) & 0xF ) >= 16 - $self->{'colorsPerPalette'} ) {
# 	    $self->AddColor(substr($paletteData,$paletteDataByte,2));
# 	}
#     }
#     close PALETTE;

#     if ( $tmpFileRemove ) {
# 	unlink $tmpFile;
#     }

#     $self->{'colorsCountRead'} = $self->{'colorsCount'};
# }

# sub OutputPalette {
#     my ($self) = @_;
#     $self->OutputPaletteForFileAndBinMode($self->{'paletteFileOutputAsm'},0);
#     $self->OutputPaletteForFileAndBinMode($self->{'paletteFileOutputBin'},1);
# }

# sub OutputPaletteForFileAndBinMode {
#     my ($self,$file,$bin) = @_;
#     if ( $file eq '' ) {
# 	return;
#     }
#     my $fileContents = $self->GetOutputPaletteData($bin);
#     open(PALETTE,'>'.$file);
#     if ( $bin ) {
# 	binmode PALETTE;
#     }
#     print PALETTE $fileContents;
#     close PALETTE;
# }

# sub GetOutputPaletteData {
#     my ($self,$bin) = @_;
#     if ( $self->{'colorsCount'} <= 0 ) {
# 	return '';
#     }
#     my $fileContents = '';
#     if ( ! $bin ) {
# 	if ( $self->{'comments'} ) {
# 	    $fileContents .= ";; palette\n";
# 	}
# 	if ( $self->{'labels'} ) {
# 	    $fileContents .= "Start".$self->{'label'}."Palette:\n";
# 	}
#     }
#     for ( my $paletteIndex = 0; $paletteIndex < $self->{'paletteCount'}; $paletteIndex++ ) {
# 	for ( my $i = 0; $i < (16-$self->{'colorsPerPalette'}); $i++ ) {
# 	    if ( $bin ) {
# 		$fileContents .= pack("n",0x0000);
# 	    } else {   
# 		$fileContents .= " dc.w \$0000\n";
# 	    }
# 	}
# 	for ( my $i = 0; $i < $self->{'colorsPerPalette'}; $i++ ) {
# 	    my $colorsIndex = $paletteIndex * $self->{'colorsPerPalette'} + $i;
# 	    if ( $colorsIndex < $self->{'colorsCount'} ) {
# 		if ( $bin ) {
# 		    $fileContents .= $self->{'colors'}[$colorsIndex];
# 		} else {
# 		    my $colorf = sprintf("\$%4.4X",unpack("n",$self->{'colors'}[$colorsIndex]));
# 		    $fileContents .= ' dc.w '.$colorf."\n";
# 		}
# 	    } else {
# 		if ( $bin ) {
# 		    $fileContents .= pack("n",0x0000);
# 		} else {
# 		    $fileContents .= " dc.w \$0000\n";
# 		}
# 	    }
# 	}
#     }
#     if ( ! $bin && $self->{'labels'} ) {
# 	$fileContents .= "End".$self->{'label'}."Palette:\n";
#     }
#     return $fileContents;
# }

# my %cache;

# sub GetColorIndex {
#     my ($self,$color) = @_;
#     my $index = -1;
#     if ( ( $self->{'magicPink'} && $color eq $constantColorPink ) ||
# 	 ( $self->{'magicBlack'} && $color eq $constantColorBlack ) ) {
# 	$index = 0;
#     } else {
# 	$index = $self->GetColorIndexWithNoAdding($color);
# 	if ( $index < 0 ) {
# 	    $index = $self->AddColor($color);
# 	}
#     }
#     return $index;
# }

# sub GetColorIndexWithNoAdding {
#     my ($self,$color) = @_;
#     my $index = -1;
#     my $colorsCount = $self->{'colorsCount'};
#     for ( my $colorsIndex = 0; $colorsIndex < $colorsCount; $colorsIndex++ ) {
# 	if ( $color eq $self->{'colors'}[$colorsIndex] ) {
# 	    # account for each palette having unused transparent index
# 	    $index = ( $colorsIndex % $self->{'colorsPerPalette'} ) + 16 * int($colorsIndex / $self->{'colorsPerPalette'}) + (16-$self->{'colorsPerPalette'});
# 	    last;
# 	}
#     }
#     return $index;
# }

# sub AddColor {
#     my ($self,$color) = @_;
#     my $index = -1;
#     if ( $self->{'colorsCount'} < $self->{'colorsPerPalette'} * $self->{'paletteCount'} ) {
# 	push @{$self->{'colors'}}, $color;
# 	my $colorsIndex = $self->{'colorsCount'};
# 	$index = ( $colorsIndex % $self->{'colorsPerPalette'} ) + 16 * int($colorsIndex / $self->{'colorsPerPalette'}) + (16-$self->{'colorsPerPalette'});
# 	$self->{'colorsCount'}++;
# 	# debug print
# 	# printf("Adding color $index : \$%4.4X\n",unpack("n",$color));
#     } else {
# 	$self->{'colorsMissing'}{$color}++;
# 	$index = -1;
#     }
#     return $index;
# }

# sub AddColorsFromImageMagickImage {
#     my ($self,$img) = @_;
#     use Image::Magick;

#     my $width = $img->Get('width');
#     my $height = $img->Get('height');
#     my $alpha = $img->Get('matte');

#     # used to keep track of frequency of colors
#     my @colors;

#     for ( my $i = 0; $i < 8*8*8; $i++ ) {
# 	$colors[$i] = 0;
#     }

#     for ( my $y = 0; $y < $height; $y++ ) {
# 	for ( my $x = 0; $x < $width; $x++ ) {
# 	    my $a = 0;
# 	    if ( $alpha ) {
# 		$a = $img->GetPixel('channel'=>'Alpha','normalize'=>1,'x'=>$x,'y'=>$y);
# 		# TODO 95% or 100% or something else?
# 		if ( defined $a && $a > 0.95 ) {
# 		    next;
# 		}
# 	    }
# 	    my ($r,$g,$b) = $img->GetPixel('channel'=>'RGB','normalize'=>1,'x'=>$x,'y'=>$y);
# 	    # 15% performance optimization: use simpler format to store color
# 	    my $colorInt = &SCDTools::Palette::ConvertRGBNormalizedToVDPColorInt($r,$g,$b);
# 	    $colors[$colorInt]++;
# 	}
#     }

#     # add colors in order of most used to least used
#     foreach my $colorInt ( sort { $colors[$b] <=> $colors[$a] || $a <=> $b } (0..8*8*8-1) ) {
# 	if ( $colors[$colorInt] == 0 ) {
# 	    last;
# 	}
# 	my $color = &SCDTools::Palette::ConvertVDPColorIntToVDPColor($colorInt);
# 	$self->GetColorIndex($color);
#     }
# }

# sub UpdateImageMagickPalette {
#     my ($self,$imgPalette) = @_;
#     use Image::Magick;

#     my $colorsCount = $self->{'colorsCount'};

#     $imgPalette->Set(size=>"${colorsCount}x1");
#     my $x = $imgPalette->ReadImage('xc:black');
#     warn $x if $x;

#     for ( my $colorsIndex = 0; $colorsIndex < $colorsCount; $colorsIndex++ ) {
# 	my $color = $self->{'colors'}[$colorsIndex];
# 	my @pixels = &ConvertVDPColorToRGBNormalized($color);
# 	$imgPalette->SetPixel('channel'=>'RGB','x'=>$colorsIndex,'y'=>0,'color'=>\@pixels);
#     }
# }

# sub Convert {
#     my ($self,$type) = @_;
#     my $colorsCount = $self->{'colorsCount'};
#     for ( my $colorsIndex = 0; $colorsIndex < $colorsCount; $colorsIndex++ ) {
# 	my $cOld = $self->{'colors'}[$colorsIndex];
# 	my $cNew;
# 	if ( $type eq 'greyavg' ) {
# 	    my ($r,$g,$b) = &ConvertVDPColorToRGBNormalized($cOld);
# 	    my $avg = ($r+$g+$b)/3.0;
# 	    $cNew = &ConvertRGBNormalizedToVDPColor($avg,$avg,$avg);
# 	} elsif ( $type eq 'desaturate50' ) {
# 	    my ($r,$g,$b) = &ConvertVDPColorToRGBNormalized($cOld);
# 	    my $avg = ($r+$g+$b)/3.0;
# 	    $cNew = &ConvertRGBNormalizedToVDPColor(0.5*($r+$avg),0.5*($g+$avg),0.5*($b+$avg));
# 	} elsif ( $type eq 'invert' ) {
# 	    my ($r,$g,$b) = &ConvertVDPColorToRGBNormalized($cOld);
# 	    my $avg = ($r+$g+$b)/3.0;
# 	    $cNew = &ConvertRGBNormalizedToVDPColor(1.0-$r,1.0-$g,1.0-$b);
# 	} elsif ( $type eq 'redonly' ) {
# 	    my ($r,$g,$b) = &ConvertVDPColorToRGBNormalized($cOld);
# 	    my $avg = ($r+$g+$b)/3.0;
# 	    $cNew = &ConvertRGBNormalizedToVDPColor($r,0,0);
# 	} elsif ( $type eq 'greenonly' ) {
# 	    my ($r,$g,$b) = &ConvertVDPColorToRGBNormalized($cOld);
# 	    my $avg = ($r+$g+$b)/3.0;
# 	    $cNew = &ConvertRGBNormalizedToVDPColor(0,$g,0);
# 	} elsif ( $type eq 'blueonly' ) {
# 	    my ($r,$g,$b) = &ConvertVDPColorToRGBNormalized($cOld);
# 	    my $avg = ($r+$g+$b)/3.0;
# 	    $cNew = &ConvertRGBNormalizedToVDPColor(0,0,$b);
# 	} else {
# 	    $cNew = $cOld;
# 	}
# 	$self->{'colors'}[$colorsIndex] = $cNew;
#     }
# }

# sub ConvertRGBNormalizedToVDPColorInt {
#     my ($r,$g,$b) = @_;
#     return (int((255*$b)>>5)<<6)|(int((255*$g)>>5)<<3)|(int((255*$r)>>5));
# }

# sub ConvertVDPColorIntToVDPColor {
#     my ($colorInt) = @_;
#     return chr(0x0E & ($colorInt>>5)).chr((0xE0 & ($colorInt<<2))|(0x0E & ($colorInt<<1)));
# }

# sub ConvertRGBNormalizedToVDPColor {
#     my ($r,$g,$b) = @_;
#     return chr(int((255*$b)>>5)<<1).chr((int((255*$g)>>5)<<5)|(int((255*$r)>>5)<<1));
# }

# sub ConvertVDPColorToRGBNormalized {
#     my ($c) = @_;
#     # TODO inverse it
#     # pixel: 0-7
#     # (pixel*32/255) is lowest part
#     # add 1/16 to get to highlight
#     # add 1/32 to get halway between regular and highlight
#     return ((ord(substr($c,1,1))&0xE)/15.0,((ord(substr($c,1,1))>>4)&0xE)/15.0,(ord(substr($c,0,1))&0xE)/15.0);
# }

# sub ConvertVDPColorToRGB24Bit {
#     my ($c) = @_;
#     return chr(((ord(substr($c,1,1))&0xE)/15.0)*255).chr((((ord(substr($c,1,1))>>4)&0xE)/15.0)*255).chr(((ord(substr($c,0,1))&0xE)/15.0)*255);
# }

# sub PrintMissingColors {
#     my ($self) = @_;
#     my @colors = sort {$self->{'colorsMissing'}{$b} <=> $self->{'colorsMissing'}{$a}} keys %{$self->{'colorsMissing'}};
#     foreach my $color (@colors) {
# 	my $count = $self->{'colorsMissing'}{$color};
# 	my $colorf = sprintf("\$%4.4X",unpack("n",$color));
# 	print STDERR "Palette does not contain color: $colorf ($count)\n";
#     }
# }

# 1;



# ###############################################################################
# # OLD CODE that might be useful to reinstate
# ###############################################################################

# # if ( $missingColors ) {
# #     print STDERR "Colors detected exceeded colors allowed in palette\n";
# # }

# # my @colors = sort {$missingColors{$b} <=> $missingColors{$a}} keys %missingColors;

# # if ( $paletteGet ) {
# #     my $index = 0;
# #     if ( $#colors + 1 > $paletteCount * 16 - 1 ) {
# # 	print STDERR "Too many colors for one palette (will use most frequent colors)\n";
# #     }
# #     for ( my $paletteIndex = 0; $paletteIndex < $paletteCount; $paletteIndex++ ) {
# # 	if ( $bin ) {
# # 	    print pack("n",0x0000);
# # 	} else {   
# # 	    if ( $comments ) {
# # 		print ";; palette\n";
# # 	    }
# # 	    print " dc.w \$0000\n";
# # 	}
# # 	for ( my $i = 0; $i < 15; $i++ ) {
# # 	    my $colorsIndex = $paletteIndex * 15 + $i;
# # 	    if ( $colorsIndex <= $#colors ) {
# # 		if ( $bin ) {
# # 		    print $colors[$colorsIndex];
# # 		} else {
# # 		    my $colorf = sprintf("\$%4.4X",unpack("n",$colors[$colorsIndex]));
# # 		    print ' dc.w '.$colorf."\n";
# # 		}
# # 	    } else {
# # 		if ( $bin ) {
# # 		    print pack("n",0x0000);
# # 		} else {
# # 		    print " dc.w \$0000\n";
# # 		}
# # 	    }
# # 	}
# #     }
# # } else {
# #     foreach my $color (@colors) {
# # 	my $count = $missingColors{$color};
# # 	my $colorf = sprintf("\$%4.4X",unpack("n",$color));
# # 	print STDERR "Palette does not contain color: $colorf ($count)\n";
# #     }
# # }
