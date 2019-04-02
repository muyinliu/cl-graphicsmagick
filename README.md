# cl-graphicsmagick

**cl-graphicsmagick** is a Common Lisp CFFI wrapper for [GraphicsMagickWand](http://www.graphicsmagick.org/wand/wand.html).

## Compatibility

| Common Lisp | Linux | Mac | Unix | Windows |
|-------------|:-----:|:---:|:----:|:-------:|
| SBCL        |  Yes  | Yes |   ?  |    ?    |

Note: I don't have Unix system so haven't test on Unix yet. Welcome to reply.

## Build & Install GraphicsMagickWand dynamic library

### Linux

Note: if you want to run `cl-graphicsmagick` on Linux 64bit systems with SBCL, you should patch the source code of GraphicsMagick: delete/comment `#if defined(SIGSEGV)` statement in file `magick/magick.c` and build, more details see **Known Issues** above.

#### Build Dependencies

- freetype-devel
- zlib-devel
- libjpeg-devel
- libpng-devel
- libtiff-devel
- giflib-devel
- libtool-ltdl-devel
- lcms2-devel
- jasper-devel
- libxml2-devel
- bzip2-devel

Note: On CentOS you can just run command below command to install dependencies:

```shell
yum install -y freetype-devel zlib-devel libjpeg-devel libpng-devel libtiff-devel giflib-devel libtool-ltdl-devel lcms2-devel jasper-devel libxml2-devel bzip2-devel
```

#### Build from source code

```shell
mkdir /usr/local/src
cd /usr/local/src
wget http://downloads.sourceforge.net/project/graphicsmagick/graphicsmagick/1.3.25/GraphicsMagick-1.3.25.tar.gz
tar xvf GraphicsMagick-1.3.25.tar.gz
cd GraphicsMagick-1.3.25
```

Note: make sure comment `#if defined(SIGSEGV)` statement in file `magick/magick.c`

```shell
./configure --enable-shared \
            --disable-static \
            --with-modules \
            --without-x \
            --without-magick-plus-plus \
            --without-perl \
            --disable-openmp && \
            make && make install
sudo make && make install
```

Note: if you want to use Windows font, use option `--with-windows-font-dir`, for example: `--with-windows-font-dir=/usr/share/fonts/ms_font`(put Windows font files to path `/usr/share/fonts/ms_font`), Windows font's filenames end with `.ttf`, recommend using font file `Arial Unicode.ttf`

Note: Make sure file `libGraphicsMagickWand.so` and `libGraphicsMagick.so` can be found in one of these location: `/usr/lib` `/usr/local/lib`

### Mac OS X

Please do use [HomeBrew](https://brew.sh) to install GraphicsMagick:

```shell
brew install graphicsmagick
```

## Install and load cl-graphicsmagick with QuickLisp

In shell:

```shell
git clone https://github.com/muyinliu/cl-graphicsmagick.git
cp -r cl-diskspace ~/quicklisp/local-projects/
```

In Common Lisp:

```lisp
(ql:quickload 'cl-graphicsmagick)
```

Note: `gm` is the nickname of package `cl-graphicsmagick`

## Usage

### Get GraphicsMagick's version

```lisp
(gm:magick-get-version)
```
=>
```=>
"GraphicsMagick 1.3.25 2016-09-05 Q8 http://www.GraphicsMagick.org/"
1578241
```

### Get Image's information

```lisp
(gm:describe-image "/path/to/image.png")
```
=>
```=>
"Image: /path/to/image.png
  Format: PNG (Portable Network Graphics)
  Geometry: 99x99
  Class: DirectClass
  Type: true color with transparency
  Depth: 8 bits-per-pixel component
  Channel Depths:
    Red:      8 bits
    Green:    8 bits
    Blue:     8 bits
    Opacity:  1 bits
  Channel Statistics:
    Red:
      Minimum:                     0.00 (0.0000)
      Maximum:                   255.00 (1.0000)
      Mean:                      216.70 (0.8498)
      Standard Deviation:         54.02 (0.2118)
    Green:
      Minimum:                     0.00 (0.0000)
      Maximum:                   255.00 (1.0000)
      Mean:                      169.32 (0.6640)
      Standard Deviation:         62.09 (0.2435)
    Blue:
      Minimum:                     0.00 (0.0000)
      Maximum:                   255.00 (1.0000)
      Mean:                      136.35 (0.5347)
      Standard Deviation:         75.14 (0.2947)
    Opacity:
      Minimum:                     0.00 (0.0000)
      Maximum:                   255.00 (1.0000)
      Mean:                       31.79 (0.1247)
      Standard Deviation:         84.25 (0.3304)
  Opacity: (255,255,255,255)   #FFFFFFFF
  Rendering-Intent: saturation
  Gamma: 0.45455
  Chromaticity:
    red primary: (0.64,0.33)
    green primary: (0.3,0.6)
    blue primary: (0.15,0.06)
    white point: (0.3127,0.329)
  Resolution: 28.35x28.35 pixels/centimeter
  Filesize: 16.7Ki
  Interlace: No
  Orientation: Unknown
  Background Color: white
  Border Color: #DFDFDF00
  Matte Color: #BDBDBD00
  Page geometry: 99x99+0+0
  Compose: Over
  Dispose: Undefined
  Iterations: 0
  Compression: Zip
  Png:IHDR.color-type-orig: 6
  Png:IHDR.bit-depth-orig: 8
  Signature: 22840ae27432c17cf141a0abbc0c892f85b16ffadd793d5caccc57b5744f40ca
  Profile-color: 3144 bytes
  Tainted: False
  User Time: 162.980u
  Elapsed Time: 223:34
  Pixels Per Second: 0
"
```

### Get image's width and height

```lisp
(gm:image-width-height "/1.png")
```
=>
```=>
99
99
```

### Rotate image

```lisp
(gm:rotate-image "/input.png" "/rotate_output.png" "white" 45)
```
=>
```=>
"/rotate_output.png"
```

### Resize image

```lisp
(gm:resize-image "/input.png" "/resize_output_30x30px.png" 30 30)
```
=>
```=>
"/resize_output_30x30px.png"
```

### Add text watermark to image

```lisp
(gm:add-text-watermark-to-image "/input.png" 
                                "/add_text_watermarks_output.png"
                                "@muyinliu"
                                :color "white"
                                :font-path "/path/to/font.otf"
                                :font-size 14
                                :gravity :%SouthEaseGravity
                                :x 10
                                :y 10)
```
=>
```=>
"/add_text_watermarks_output.png"
```


-----------------------------------------------------------------
## Run test case

### Dependencies

- [prove](https://github.com/fukamachi/prove)
- [cl-ansi-text](https://github.com/pnathan/cl-ansi-text)

Install dependencies with [Quicklisp](http://www.quicklisp.org):

```lisp
(ql:quickload '(prove prove-asdf cl-ansi-text))
```

Note: make sure have all dependencies installed.

### Run test case now

In Common Lisp REPL:

```lisp
(asdf:test-system :cl-graphicsmagick)
```

OR in Shell:

```shell
sbcl --eval "(asdf:test-system :cl-graphicsmagick)" --eval "(quit)"
```

-----------------------------------------------------------------
## Known Issues

### SIGSEGV Signal Handlers bug while runing on SBCL with Linux 64bit systems

Both GraphicsMagick and SBCL want to handle the signal cause SBCL crash on Linux 64bit systems(as far as I know, `cl-graphicsmagick` crash on CentOS 5.8 Final 64bit and CentOS 6.7 Final 64bit, and stuck CPU 100% on Debian 64bit system).

Thanks to **mordocai**, **stassats** and **foom**, solution is found.

Please delete/comment these lines(near line 1060) in `magick/magick.c` of GraphicsMagick v1.3.18+:

```c
#if defined(SIGSEGV)
  (void) MagickCondSignal(SIGSEGV,MagickPanicSignalHandler);
#endif
```

And rebuild GraphicsMagick.

### Unable to read font (/usr/local/share/ghostscript/fonts/n019003l.pfb)

Install ghostscript will solve this problem.

#### On Mac OS

```shell
brew install ghostscript
```

#### On Linux

```shell
yum install ghostscript
```
OR
```shell
apt-get install ghostscript
```

## License

MIT (See LICENSE file for details).
