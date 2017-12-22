(in-package :gm)

;;; Common types

(define-foreign-type double-type ()
  nil
  (:actual-type :double)
  (:simple-parser %double))

(defmethod translate-to-foreign (value (type double-type))
  (coerce value 'double-float))

;;; Enum definitions

;; magick/colorspace.h
(defcenum %ColorspaceType
  :%UndefinedColorspace
  :%RGBColorspace         ;; Plain old RGB colorspace
  :%GRAYColorspace        ;; Plain old full-range grayscale
  :%TransparentColorspace ;; RGB but preserve matte channel during quantize
  :%OHTAColorspace
  :%XYZColorspace         ;; CIE XYZ
  :%YCCColorspace         ;; Kodak PhotoCD PhotoYCC
  :%YIQColorspace
  :%YPbPrColorspace
  :%YUVColorspace
  :%CMYKColorspace        ;; Cyan, magenta, yellow, black, alpha
  :%sRGBColorspace        ;; Kodak PhotoCD sRGB
  :%HSLColorspace         ;; Hue, saturation, luminosity
  :%HWBColorspace         ;; Hue, whiteness, blackness
  :%LABColorspace         ;; LAB colorspace not supported yet other than via lcms
  :%CineonLogRGBColorspace;; RGB data with Cineon Log scaling, 2.048 density range
  :%Rec601LumaColorspace  ;; Luma (Y) according to ITU-R 601
  :%Rec601YCbCrColorspace ;; YCbCr according to ITU-R 601
  :%Rec709LumaColorspace  ;; Luma (Y) according to ITU-R 709
  :%Rec709YCbCrColorspace);; YCbCr according to ITU-R 709

;; magick/compare.h
(defcenum %MetricType
  :%UndefinedMetric
  :%MeanAbsoluteErrorMetric
  :%MeanSquaredErrorMetric
  :%PeakAbsoluteErrorMetric
  :%PeakSignalToNoiseRatioMetric
  :%RootMeanSquaredErrorMetric)

;; magick/constitute.h
(defcenum %StorageType
  :%CharPixel         ;; Unsigned 8 bit 'unsigned char'
  :%ShortPixel        ;; Unsigned 16 bit 'unsigned short int'
  :%IntegerPixel      ;; Unsigned 32 bit 'unsigned int'
  :%LongPixel         ;; Unsigned 32 or 64 bit (CPU dependent) 'unsigned long'
  :%FloatPixel        ;; Floating point 32-bit 'float'
  :%DoublePixel)      ;; Floating point 64-bit 'double'

;; magick/error.h

(defcenum %ExceptionType
  (:%UndefinedException        0)
  (:%WarningException          300) ;; warning exceptions
  (:%ResourceLimitWarning      300)
  (:%TypeWarning               305)
  (:%OptionWarning             310)
  (:%DelegateWarning           315)
  (:%MissingDelegateWarning    320)
  (:%CorruptImageWarning       325)
  (:%FileOpenWarning           330)
  (:%BlobWarning               335)
  (:%StreamWarning             340)
  (:%CacheWarning              345)
  (:%CoderWarning              350)
  (:%ModuleWarning             355)
  (:%DrawWarning               360)
  (:%ImageWarning              365)
  (:%XServerWarning            380)
  (:%MonitorWarning            385)
  (:%RegistryWarning           390)
  (:%ConfigureWarning          395)
  (:%ErrorException            400) ;; error exceptions
  (:%ResourceLimitError        400)
  (:%TypeError                 405)
  (:%OptionError               410)
  (:%DelegateError             415)
  (:%MissingDelegateError      420)
  (:%CorruptImageError         425)
  (:%FileOpenError             430)
  (:%BlobError                 435)
  (:%StreamError               440)
  (:%CacheError                445)
  (:%CoderError                450)
  (:%ModuleError               455)
  (:%DrawError                 460)
  (:%ImageError                465)
  (:%WandError                 467)
  (:%XServerError              480)
  (:%MonitorError              485)
  (:%RegistryError             490)
  (:%ConfigureError            495)
  (:%FatalErrorException       700) ;; fatal error exceptions
  (:%ResourceLimitFatalError   700)
  (:%TypeFatalError            705)
  (:%OptionFatalError          710)
  (:%DelegateFatalError        715)
  (:%MissingDelegateFatalError 720)
  (:%CorruptImageFatalError    725)
  (:%FileOpenFatalError        730)
  (:%BlobFatalError            735)
  (:%StreamFatalError          740)
  (:%CacheFatalError           745)
  (:%CoderFatalError           750)
  (:%ModuleFatalError          755)
  (:%DrawFatalError            760)
  (:%ImageFatalError           765)
  (:%XServerFatalError         780)
  (:%MonitorFatalError         785)
  (:%RegistryFatalError        790)
  (:%ConfigureFatalError       795))

;; magick/image.h
(defcenum %AlphaType
  :%UnspecifiedAlpha
  :%AssociatedAlpha
  :%UnassociatedAlpha)

(defcenum %ChannelType
  :%UndefinedChannel
  :%RedChannel     ;; RGB Red channel
  :%CyanChannel    ;; CMYK Cyan channel
  :%GreenChannel   ;; RGB Green channel
  :%MagentaChannel ;; CMYK Magenta channel
  :%BlueChannel    ;; RGB Blue channel
  :%YellowChannel  ;; CMYK Yellow channel
  :%OpacityChannel ;; Opacity channel
  :%BlackChannel   ;; CMYK Black (K) channel
  :%MatteChannel   ;; Same as Opacity channel (deprecated)
  :%AllChannels    ;; Color channels
  :%GrayChannel)   ;; Color channels represent an intensity.

(defcenum %ClassType
  :%UndefinedClass
  :%DirectClass
  :%PseudoClass)

(defcenum %CompositeOperator
  (:%UndefinedCompositeOp 0)
  :%OverCompositeOp
  :%InCompositeOp
  :%OutCompositeOp
  :%AtopCompositeOp
  :%XorCompositeOp
  :%PlusCompositeOp
  :%MinusCompositeOp
  :%AddCompositeOp
  :%SubtractCompositeOp
  :%DifferenceCompositeOp
  :%MultiplyCompositeOp
  :%BumpmapCompositeOp
  :%CopyCompositeOp
  :%CopyRedCompositeOp
  :%CopyGreenCompositeOp
  :%CopyBlueCompositeOp
  :%CopyOpacityCompositeOp
  :%ClearCompositeOp
  :%DissolveCompositeOp
  :%DisplaceCompositeOp
  :%ModulateCompositeOp
  :%ThresholdCompositeOp
  :%NoCompositeOp
  :%DarkenCompositeOp
  :%LightenCompositeOp
  :%HueCompositeOp
  :%SaturateCompositeOp
  :%ColorizeCompositeOp
  :%LuminizeCompositeOp
  :%ScreenCompositeOp
  :%OverlayCompositeOp
  :%CopyCyanCompositeOp
  :%CopyMagentaCompositeOp
  :%CopyYellowCompositeOp
  :%CopyBlackCompositeOp
  :%DivideCompositeOp
  :%HardLightCompositeOp
  :%ExclusionCompositeOp
  :%ColorDodgeCompositeOp
  :%ColorBurnCompositeOp
  :%SoftLightCompositeOp
  :%LinearBurnCompositeOp
  :%LinearDodgeCompositeOp
  :%LinearLightCompositeOp
  :%VividLightCompositeOp
  :%PinLightCompositeOp
  :%HardMixCompositeOp)

(defcenum %CompressionType
  :%UndefinedCompression
  :%NoCompression
  :%BZipCompression
  :%FaxCompression
  :%Group3Compression ;; = FaxCompression
  :%Group4Compression
  :%JPEGCompression
  :%LosslessJPEGCompression
  :%LZWCompression
  :%RLECompression
  :%ZipCompression
  :%LZMACompression              ;; Lempel-Ziv-Markov chain algorithm
  :%JPEG2000Compression          ;; ISO/IEC std 15444-1
  :%JBIG1Compression             ;; ISO/IEC std 11544 / ITU-T rec T.82
  :%JBIG2Compression)            ;; ISO/IEC std 14492 / ITU-T rec T.88

(defcenum %DisposeType
  :%UndefinedDispose
  :%NoneDispose
  :%BackgroundDispose
  :%PreviousDispose)

(defcenum %EndianType 
  :%UndefinedEndian
  :%LSBEndian            ;; "little" endian
  :%MSBEndian            ;; "big" endian
  :%NativeEndian)        ;; native endian

(defcenum %FilterTypes 
  :%UndefinedFilter
  :%PointFilter
  :%BoxFilter
  :%TriangleFilter
  :%HermiteFilter
  :%HanningFilter
  :%HammingFilter
  :%BlackmanFilter
  :%GaussianFilter
  :%QuadraticFilter
  :%CubicFilter
  :%CatromFilter
  :%MitchellFilter
  :%LanczosFilter
  :%BesselFilter
  :%SincFilter)

(defcenum %GeometryFlags
  (:%NoValue      #x00000)
  (:%XValue       #x00001)
  (:%YValue       #x00002)
  (:%WidthValue   #x00004)
  (:%HeightValue  #x00008)
  (:%AllValues    #x0000F)
  (:%XNegative    #x00010)
  (:%YNegative    #x00020)
  (:%PercentValue #x01000) ;; %
  (:%AspectValue  #x02000) ;; !
  (:%LessValue    #x04000) ;; <
  (:%GreaterValue #x08000) ;; >
  (:%AreaValue    #x10000) ;; @
  (:%MinimumValue #x20000));; ^

(defcenum %GravityType 
  :%ForgetGravity
  :%NorthWestGravity
  :%NorthGravity
  :%NorthEastGravity
  :%WestGravity
  :%CenterGravity
  :%EastGravity
  :%SouthWestGravity
  :%SouthGravity
  :%SouthEastGravity
  :%StaticGravity)

(defcenum %ImageType 
  :%UndefinedType
  :%BilevelType
  :%GrayscaleType
  :%GrayscaleMatteType
  :%PaletteType
  :%PaletteMatteType
  :%TrueColorType
  :%TrueColorMatteType
  :%ColorSeparationType
  :%ColorSeparationMatteType
  :%OptimizeType)

(defcenum %InterlaceType 
  :%UndefinedInterlace
  :%NoInterlace
  :%LineInterlace
  :%PlaneInterlace
  :%PartitionInterlace)

(defcenum %MontageMode 
  :%UndefinedMode
  :%FrameMode
  :%UnframeMode
  :%ConcatenateMode)

(defcenum %NoiseType
  :%UniformNoise
  :%GaussianNoise
  :%MultiplicativeGaussianNoise
  :%ImpulseNoise
  :%LaplacianNoise
  :%PoissonNoise
  :%RandomNoise
  :%UndefinedNoise)

;; Image orientation.  Based on TIFF standard values (also EXIF).
(defcenum %OrientationType  ;; Line direction | Frame Direction
                            ;; ---------------|----------------
  :%UndefinedOrientation    ;; Unknown        | Unknown        
  :%TopLeftOrientation      ;; Left to right  | Top to bottom  
  :%TopRightOrientation     ;; Right to left  | Top to bottom  
  :%BottomRightOrientation  ;; Right to left  | Bottom to top  
  :%BottomLeftOrientation   ;; Left to right  | Bottom to top  
  :%LeftTopOrientation      ;; Top to bottom  | Left to right  
  :%RightTopOrientation     ;; Top to bottom  | Right to left  
  :%RightBottomOrientation  ;; Bottom to top  | Right to left  
  :%LeftBottomOrientation)  ;; Bottom to top  | Left to right  

(defcenum %PreviewType 
  :%UndefinedPreview
  :%RotatePreview
  :%ShearPreview
  :%RollPreview
  :%HuePreview
  :%SaturationPreview
  :%BrightnessPreview
  :%GammaPreview
  :%SpiffPreview
  :%DullPreview
  :%GrayscalePreview
  :%QuantizePreview
  :%DespecklePreview
  :%ReduceNoisePreview
  :%AddNoisePreview
  :%SharpenPreview
  :%BlurPreview
  :%ThresholdPreview
  :%EdgeDetectPreview
  :%SpreadPreview
  :%SolarizePreview
  :%ShadePreview
  :%RaisePreview
  :%SegmentPreview
  :%SwirlPreview
  :%ImplodePreview
  :%WavePreview
  :%OilPaintPreview
  :%CharcoalDrawingPreview
  :%JPEGPreview)

(defcenum %RenderingIntent 
  :%UndefinedIntent
  :%SaturationIntent
  :%PerceptualIntent
  :%AbsoluteIntent
  :%RelativeIntent)

(defcenum %ResolutionType 
  :%UndefinedResolution
  :%PixelsPerInchResolution
  :%PixelsPerCentimeterResolution)

;; magick/operator.h
(defcenum %QuantumOperator
  :%UndefinedQuantumOp
  :%AddQuantumOp                   ;; Add value 
  :%AndQuantumOp                   ;; Bitwise AND value 
  :%AssignQuantumOp                ;; Direct value assignment 
  :%DivideQuantumOp                ;; Divide by value 
  :%LShiftQuantumOp                ;; Bitwise left-shift value N bits 
  :%MultiplyQuantumOp              ;; Multiply by value 
  :%OrQuantumOp                    ;; Bitwise OR value 
  :%RShiftQuantumOp                ;; Bitwise right shift value 
  :%SubtractQuantumOp              ;; Subtract value 
  :%ThresholdQuantumOp             ;; Above threshold white otherwise black 
  :%ThresholdBlackQuantumOp        ;; Below threshold is black 
  :%ThresholdWhiteQuantumOp        ;; Above threshold is white 
  :%XorQuantumOp                   ;; Bitwise XOR value 
  :%NoiseGaussianQuantumOp         ;; Gaussian noise 
  :%NoiseImpulseQuantumOp          ;; Impulse noise 
  :%NoiseLaplacianQuantumOp        ;; Laplacian noise 
  :%NoiseMultiplicativeQuantumOp   ;; Multiplicative gaussian noise 
  :%NoisePoissonQuantumOp          ;; Poisson noise 
  :%NoiseUniformQuantumOp          ;; Uniform noise 
  :%NegateQuantumOp                ;; Negate (invert) channel ignore value 
  :%GammaQuantumOp                 ;; Adjust image gamma 
  :%DepthQuantumOp                 ;; Adjust image depth 
  ;; Below added on 2008-12-13 
  :%LogQuantumOp                   ;; log(quantum*value+1)/log(value+1) 
  :%MaxQuantumOp                   ;; Assign value if > quantum 
  :%MinQuantumOp                   ;; Assign value if < quantum 
  :%PowQuantumOp                   ;; Power function: pow(quantum,value) 
  ;; Below added on 2012-03-17 
  :%NoiseRandomQuantumOp           ;; Random noise 
  ;; Below added on 2014-05-17 
  :%ThresholdBlackNegateQuantumOp  ;; Below threshold is set to white 
  :%ThresholdWhiteNegateQuantumOp) ;; Above threshold is set to black 

;; magick/pixel_cache.h
(defcenum %VirtualPixelMethod
  :%UndefinedVirtualPixelMethod
  :%ConstantVirtualPixelMethod
  :%EdgeVirtualPixelMethod
  :%MirrorVirtualPixelMethod
  :%TileVirtualPixelMethod)

;; magick/render.h
(defcenum %FillRule
  :%UndefinedRule
  :%EvenOddRule
  :%NonZeroRule)

(defcenum %LineCap
  :%UndefinedCap
  :%ButtCap
  :%RoundCap
  :%SquareCap)

(defcenum %LineJoin
  :%UndefinedJoin
  :%MiterJoin
  :%RoundJoin
  :%BevelJoin)

(defcenum %ClipPathUnits
  :%UserSpace
  :%UserSpaceOnUse
  :%ObjectBoundingBox)

(defcenum %PaintMethod
  (:%PointMethod 0)
  :%ReplaceMethod
  :%FloodfillMethod
  :%FillToBorderMethod
  :%ResetMethod)

(defcenum %DecorationType
  :%NoDecoration
  :%UnderlineDecoration
  :%OverlineDecoration
  :%LineThroughDecoration)

;; magick/resource.h
(defcenum %ResourceType
  :%UndefinedResource=0 ;; Undefined value
  :%DiskResource        ;; Pixel cache total disk space (Gigabytes)
  :%FileResource        ;; Pixel cache number of open files (Files)
  :%MapResource         ;; Pixel cache total file memory-mapping (Megabytes)
  :%MemoryResource      ;; Maximum pixel cache heap memory allocations (Megabytes)
  :%PixelsResource      ;; Maximum number of pixels in single image (Pixels)
  :%ThreadsResource     ;; Maximum number of worker threads
  :%WidthResource       ;; Maximum pixel width of an image (Pixels)
  :%HeightResource)     ;; Maximum pixel height of an image (Pixels)

;; magick/type.h
(defcenum %StretchType
  :%NormalStretch
  :%UltraCondensedStretch
  :%ExtraCondensedStretch
  :%CondensedStretch
  :%SemiCondensedStretch
  :%SemiExpandedStretch
  :%ExpandedStretch
  :%ExtraExpandedStretch
  :%UltraExpandedStretch
  :%AnyStretch)

(defcenum %StyleType
  :%NormalStyle
  :%ItalicStyle
  :%ObliqueStyle
  :%AnyStyle)

;; wand/drawing_wand.c
(defcenum %PathMode
  :%DefaultPathMode
  :%AbsolutePathMode
  :%RelativePathMode)


;; magick/common.h
;;
;; #define MagickPass     1
(defconstant +magick-pass+ 1)

;; magick/common.h
;;
;; #define MagickPassFail unsigned int
(defctype %MagickPassFail (:unsigned-int))

;; magick/common.h
;;
;; #define MagickBool     unsigned int
(defctype %MagickBool (:unsigned-int))

;;; Type definitions

;; magick/error.h
;;
;; typedef struct _ExceptionInfo
;; {
;;   /*
;;     Exception severity, reason, and description
;;   */
;;   ExceptionType
;;     severity;
;;
;;   char
;;     *reason,
;;     *description;
;;
;;   /*
;;     Value of errno (or equivalent) when exception was thrown.
;;   */
;;   int
;;     error_number;
;;
;;   /*
;;     Reporting source module, function (if available), and source
;;     module line.
;;   */
;;   char
;;     *module,
;;     *function;
;;
;;   unsigned long
;;     line;
;;
;;   /*
;;     Structure sanity check
;;   */
;;   unsigned long
;;     signature;
;; } ExceptionInfo;
(defctype %ExceptionInfo (:pointer))

;; magick/image.h
(defctype %Image (:pointer))

;; magick/image.h
;;
;; typedef unsigned int Quantum;
(defctype %Quantum (:unsigned-int))

;; wand/magick_wand.h
;;
;; #define MagickSizeType magick_int64_t
;; typedef signed __int64  magick_int64_t;
;; // -9,223,372,036,854,775,807 to 9,223,372,036,854,775,807
(defctype %MagickSizeType (:long))

;; wand/pixel_wand.h
(defctype %PixelWand (:pointer))

;; wand/magick_wand.c
;;
;; struct _MagickWand
;; {
;;   char
;;     id[MaxTextExtent];
;;
;;   ExceptionInfo
;;     exception;
;;
;;   ImageInfo
;;     *image_info;
;;
;;   QuantizeInfo
;;     *quantize_info;
;;
;;   Image
;;     *image,             /* Current working image */
;;     *images;            /* Whole image list */
;;
;;   unsigned int
;;     iterator;
;;
;;   unsigned long
;;     signature;
;; };
;; typedef struct _MagickWand MagickWand
(defctype %MagickWand (:pointer))

;; wand/drawing_wand.c
;;
;; struct _DrawingWand
;; {
;;   /* Support structures */
;;   ExceptionInfo
;;     exception;
;;
;;   Image
;;     *image;
;;
;;   MagickBool
;;     own_image;          /* If true, then we own the image. */
;;
;;   /* MVG output string and housekeeping */
;;   char
;;     *mvg;               /* MVG data */
;;
;;   size_t
;;     mvg_alloc,          /* total allocated memory */
;;     mvg_length;         /* total MVG length */
;;
;;   unsigned int
;;     mvg_width;          /* current line length */
;;
;;   /* Pattern support */
;;   char
;;     *pattern_id;
;;
;;   RectangleInfo
;;     pattern_bounds;
;;
;;   size_t
;;     pattern_offset;
;;
;;   /* Graphic drawing_wand */
;;   int
;;     index;              /* array index */
;;
;;   DrawInfo
;;     **graphic_context;
;;
;;   int
;;     filter_off;         /* true if not filtering attributes */
;;
;;   /* Pretty-printing depth */
;;   unsigned int
;;     indent_depth;       /* number of left-hand pad characters */
;;
;;   /* Path operation support */
;;   PathOperation
;;     path_operation;
;;
;;   PathMode
;;     path_mode;
;;
;;   /* Structure unique signature */
;;   unsigned long
;;     signature;
;; };
(defctype %DrawingWand (:pointer))

;; magick/nt_base.h
;;
;; #define MAGICK_POSIX_IO_SIZE_T unsigned int
;; #define MAGICK_POSIX_IO_SIZE_T size_t
(defctype %size_t (:unsigned-int))  ;; unsigned int

;; magick/nt_base.h
;;
;; #if !defined(ssize_t) && !defined(__MINGW32__) && !defined(__MINGW64__)
;; #  if defined(WIN64)
;;   typedef __int64 ssize_t;
;; #  else
;;   typedef int ssize_t;
;; #  endif
;; #endif /* !defined(ssize_t) && !defined(__MINGW32__) && !defined(__MINGW64__)*/
(defctype %ssize_t (:int)) ;; int/int64

;; stdio.h
(defctype %FILE (:pointer))

;; magick/image.h
;;
;; typedef struct _PixelPacket
;; {
;; #if defined(WORDS_BIGENDIAN)
;;   /* RGBA */
;; #define MAGICK_PIXELS_RGBA 1
;;   Quantum
;;     red,
;;     green,
;;     blue,
;;     opacity;
;; #else
;;   /* BGRA (as used by Microsoft Windows DIB) */
;; #define MAGICK_PIXELS_BGRA 1
;;   Quantum
;;     blue,
;;     green,
;;     red,
;;     opacity;
;; #endif
;; } PixelPacket;
(defctype %PixelPacket (:pointer))

;; magick/image.h
;; 
;; typedef struct _AffineMatrix
;; {
;;   double
;;     sx,
;;     rx,
;;     ry,
;;     sy,
;;     tx,
;;     ty;
;; } AffineMatrix;
(defctype %AffineMatrix (:pointer))

;; magick/render.h
;;
;; typedef struct _DrawInfo
;; {
;;   char
;;     *primitive,
;;     *geometry;
;;
;;   AffineMatrix
;;     affine;
;;
;;   GravityType
;;     gravity;
;;
;;   PixelPacket
;;     fill,
;;     stroke;
;;
;;   double
;;     stroke_width;
;;
;;   GradientInfo
;;     gradient;
;;
;;   Image
;;     *fill_pattern,
;;     *tile,
;;     *stroke_pattern;
;;
;;   unsigned int
;;     stroke_antialias,
;;     text_antialias;
;;
;;   FillRule
;;     fill_rule;
;;
;;   LineCap
;;     linecap;
;;
;;   LineJoin
;;     linejoin;
;;
;;   unsigned long
;;     miterlimit;
;;
;;   double
;;     dash_offset;
;;
;;   DecorationType
;;     decorate;
;;
;;   CompositeOperator
;;     compose;
;;
;;   char
;;     *text,
;;     *font,
;;     *family;
;;
;;   StyleType
;;     style;
;;
;;   StretchType
;;     stretch;
;;
;;   unsigned long
;;     weight;
;;
;;   char
;;     *encoding;
;;
;;   double
;;     pointsize;
;;
;;   char
;;     *density;
;;
;;   AlignType
;;     align;
;;
;;   PixelPacket
;;     undercolor,
;;     border_color;
;;
;;   char
;;     *server_name;
;;
;;   double
;;     *dash_pattern; /* Terminated by value 0.0 (i.e. < MagickEpsilon)*/
;;
;;   char
;;     *clip_path;
;;
;;   SegmentInfo
;;     bounds;
;;
;;   ClipPathUnits
;;     clip_units;
;;
;;   Quantum
;;     opacity;
;;
;;   unsigned int
;;     render,
;;     unused1;  /* Spare. Was long-deprecated 'debug' */
;;
;;   ElementReference
;;     element_reference;
;;
;;   unsigned long
;;     signature;
;; } DrawInfo;
(defctype %DrawInfo (:pointer))

;; magick/render.h
;;
;; typedef struct _PointInfo
;; {
;;   double
;;     x,
;;     y;
;; } PointInfo;
(defctype %PointInfo (:pointer))
