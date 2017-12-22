;;; magick_wand

(in-package :gm)

(defcfun ("InitializeMagick" %InitializeMagick)
    :void
  "Initialize GraphicsMagick environment. Must do this to prevent ERROR: Assertion failed: \(semaphore_info != \(SemaphoreInfo *) NULL), function LockSemaphoreInfo, file magick/semaphore.c, line 601.

InitializeMagick() initializes the GraphicsMagick environment.
InitializeMagick() MUST be invoked by the using program before making
use of GraphicsMagick functions or else the library will be unusable.

This function should be invoked in the primary \(original) thread of the
application's process, and before starting any OpenMP threads, as part
of program initialization.

The format of the InitializeMagick function is:
    InitializeMagick\(const char *path)
A description of each parameter follows:
  o path: The execution path of the current GraphicsMagick client.

Note: InitializeMagick is declared in file magick/magick.c

Since GraphicsMagick v1.1.0"
  (path :string))

;; InitializeMagick is called by NewMagickWand NewPixelWand & NewDrawingWand,
;;   but call InitializeMagick here to make sure all other functions
;;   like MagickQueryFormats works immediately after loaded.
(%InitializeMagick (null-pointer))


(defcfun ("CloneMagickWand" %CloneMagickWand)
    %MagickWand
  "Makes an exact copy of the specified wand.

The format of the CloneMagickWand method is:
  MagickWand *CloneMagickWand\( const MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand to clone.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("DestroyMagickWand" %DestroyMagickWand)
    :void
  "Deallocates memory associated with an MagickWand.

The format of the DestroyMagickWand method is:
  void DestroyMagickWand\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickAdaptiveThresholdImage" %MagickAdaptiveThresholdImage)
    %magick-status
  "Selects an individual threshold for each pixel based on the range of intensity values in its local neighborhood. This allows for thresholding of an image whose global intensity histogram doesn't contain distinctive peaks.

The format of the MagickAdaptiveThresholdImage method is:
  unsigned int MagickAdaptiveThresholdImage\( MagickWand *wand, const unsigned long width,
                                           const unsigned long height, const long offset );
wand:
  The magick wand.
width:
  The width of the local neighborhood.
height:
  The height of the local neighborhood.
offset:
  The mean offset.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (width :unsigned-long)
  (height :unsigned-long)
  (offset :long))

(defcfun ("MagickAddImage" %MagickAddImage)
    %magick-status
  "Adds the specified images at the current image location.

The format of the MagickAddImage method is:
  unsigned int MagickAddImage\( MagickWand *wand, const MagickWand *add_wand );
wand:
  The magick wand.
insert:
  The splice wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (add_wand %MagickWand))

(defcfun ("MagickAddNoiseImage" %MagickAddNoiseImage)
    %magick-status
  "Adds random noise to the image.

The format of the MagickAddNoiseImage method is:
  unsigned int MagickAddNoiseImage\( MagickWand *wand, const NoiseType noise_type );
wand:
  The magick wand.
noise_type:
  The type of noise: Uniform, Gaussian, Multiplicative,
Impulse, Laplacian, Poisson, or Random.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (noise_type %NoiseType))

(defcfun ("MagickAffineTransformImage" %MagickAffineTransformImage)
    %magick-status
  "Transforms an image as dictated by the affine matrix of the drawing wand.

The format of the MagickAffineTransformImage method is:
  unsigned int MagickAffineTransformImage\( MagickWand *wand, const DrawingWand *drawing_wand );
wand:
  The magick wand.
drawing_wand:
  The draw wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (drawing_wand %DrawingWand))

(defcfun ("MagickAnnotateImage" %MagickAnnotateImage)
    %magick-status
  "Annotates an image with text.

The format of the MagickAnnotateImage method is:
  unsigned int MagickAnnotateImage\( MagickWand *wand, const DrawingWand *drawing_wand,
                                  const double x, const double y, const double angle,
                                  const char *text );
wand:
  The magick wand.
drawing_wand:
  The draw wand.
x:
  x ordinate to left of text
y:
  y ordinate to text baseline
angle:
  rotate text relative to this angle.
text:
  text to draw

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (drawing_wand %DrawingWand)
  (x :double)
  (y :double)
  (angle :double)
  (text :string))

(defcfun ("MagickAnimateImages" %MagickAnimateImages)
    %magick-status
  "Animates an image or image sequence.

The format of the MagickAnimateImages method is:
  unsigned int MagickAnimateImages\( MagickWand *wand, const char *server_name );
wand:
  The magick wand.
server_name:
  The X server name.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (server_name :string))

(defcfun ("MagickAppendImages" %MagickAppendImages)
    %MagickWand
  "Append a set of images.

The format of the MagickAppendImages method is:
  MagickWand *MagickAppendImages\( MagickWand *wand, const unsigned int stack );
wand:
  The magick wand.
stack:
  By default, images are stacked left-to-right. Set stack to True
to stack them top-to-bottom.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (stack :unsigned-int))

(defcfun ("MagickAutoOrientImage" %MagickAutoOrientImage)
    %magick-status
  "Adjusts the current image so that its orientation is suitable for viewing \(i.e. top-left orientation).

The format of the MagickAutoOrientImage method is:

unsigned int MagickAutoOrientImage\( MagickWand *wand,
                                    const OrientationType current_orientation,
                                    ExceptionInfo *exception );
wand:
  The magick wand.
current_orientation:
  Current image orientation. May be one of: TopLeftOrientation Left to right and Top to bottom TopRightOrientation Right to left and Top to bottom BottomRightOrientation Right to left and Bottom to top BottomLeftOrientation Left to right and Bottom to top LeftTopOrientation Top to bottom and Left to right RightTopOrientation Top to bottom and Right to left RightBottomOrientation Bottom to top and Right to left LeftBottomOrientation Bottom to top and Left to right UndefinedOrientation Current orientation is not known. Use orientation defined by the current image if any. Equivalent to MagickGetImageOrientation\().

Returns True on success, False otherwise.

Note that after successful auto-orientation the internal orientation will be set to TopLeftOrientation. However this internal value is only written to TIFF files. For JPEG files, there is currently no support for resetting the EXIF orientation tag to TopLeft so the JPEG should be stripped or EXIF profile removed if present to prevent saved auto-oriented images from being incorrectly rotated a second time by follow-on viewers that understand the EXIF orientation tag.

Since GraphicsMagick v1.3.26"
  (wand %MagickWand)
  (current_orientation %OrientationType)
  (exception %ExceptionInfo))

(defcfun ("MagickAverageImages" %MagickAverageImages)
    %MagickWand
  "Average a set of images.

The format of the MagickAverageImages method is:
  MagickWand *MagickAverageImages\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickBlackThresholdImage" %MagickBlackThresholdImage)
    %magick-status
  "Is like MagickThresholdImage\() but forces all pixels below the threshold into black while leaving all pixels above the threshold unchanged.

The format of the MagickBlackThresholdImage method is:
  unsigned int MagickBlackThresholdImage\( MagickWand *wand, const PixelWand *threshold );
wand:
  The magick wand.
threshold:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (threshold %PixelWand))

(defcfun ("MagickBlurImage" %MagickBlurImage)
    %magick-status
  "Blurs an image. We convolve the image with a Gaussian operator of the given radius and standard deviation \(sigma). For reasonable results, the radius should be larger than sigma. Use a radius of 0 and BlurImage\() selects a suitable radius for you.

The format of the MagickBlurImage method is:
  unsigned int MagickBlurImage\( MagickWand *wand, const double radius, const double sigma );
wand:
  The magick wand.
radius:
  The radius of the Gaussian, in pixels, not counting the center
pixel.
sigma:
  The standard deviation of the Gaussian, in pixels.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (radius :double)
  (sigma :double))

(defcfun ("MagickBorderImage" %MagickBorderImage)
    %magick-status
  "Surrounds the image with a border of the color defined by the bordercolor pixel wand.

The format of the MagickBorderImage method is:
  unsigned int MagickBorderImage\( MagickWand *wand, const PixelWand *bordercolor,
                                const unsigned long width, const unsigned long height );
wand:
  The magick wand.
bordercolor:
  The border color pixel wand.
width:
  The border width.
height:
  The border height.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (bordercolor %PixelWand)
  (width :unsigned-long)
  (height :unsigned-long))

(defcfun ("MagickCdlImage" %MagickCdlImage)
    %MagickPassFail
  "The MagickCdlImage\() method applies \(\"bakes in\") the ASC-CDL which is a format for the exchange of basic primary color grading information between equipment and software from different manufacturers. The format defines the math for three functions: slope, offset and power. Each function uses a number for the red, green, and blue color channels for a total of nine numbers comprising a single color decision. A tenth number for chrominance \(saturation) has been proposed but is not yet standardized.

The cdl argument string is comma delimited and is in the form \(but
without invervening spaces or line breaks):

redslope, redoffset, redpower :
greenslope, greenoffset, greenpower :
blueslope, blueoffset, bluepower :
saturation

with the unity \(no change) specification being:

\"1.0,0.0,1.0:1.0,0.0,1.0:1.0,0.0,1.0:0.0\"

See http://en.wikipedia.org/wiki/ASC_CDL for more information.

The format of the MagickCdlImage method is:
  MagickPassFail MagickCdlImage\( MagickWand *wand, const char *cdl );

A description of each parameter follows:
wand:
  The image wand.
cdl:
  Define the coefficients for slope offset and power in the
red green and blue channels, plus saturation.

Since GraphicsMagick v1.3.8"
  (wand %MagickWand)
  (cdl :string))

(defcfun ("MagickCharcoalImage" %MagickCharcoalImage)
    %magick-status
  "Simulates a charcoal drawing.

The format of the MagickCharcoalImage method is:
  unsigned int MagickCharcoalImage\( MagickWand *wand, const double radius, const double sigma );
wand:
  The magick wand.
radius:
  The radius of the Gaussian, in pixels, not counting the center
pixel.
sigma:
  The standard deviation of the Gaussian, in pixels.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (radius :double)
  (sigma :double))

(defcfun ("MagickChopImage" %MagickChopImage)
    %magick-status
  "Removes a region of an image and collapses the image to occupy the removed portion

The format of the MagickChopImage method is:
  unsigned int MagickChopImage\( MagickWand *wand, const unsigned long width,
                              const unsigned long height, const long x, const long y );
wand:
  The magick wand.
width:
  The region width.
height:
  The region height.
x:
  The region x offset.
y:
  The region y offset.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (width :unsigned-long)
  (height :unsigned-long)
  (x :long)
  (y :long))

(defcfun ("MagickClearException" %MagickClearException)
    :void
  "Clears the last wand exception.

The format of the MagickClearException method is:

  void MagickClearException\( MagickWand *wand );

wand:
  The magick wand.

Since GraphicsMagick v1.3.26"
  (wand %MagickWand))

(defcfun ("MagickClipImage" %MagickClipImage)
    %magick-status
  "Clips along the first path from the 8BIM profile, if present.

The format of the MagickClipImage method is:
  unsigned int MagickClipImage\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickClipPathImage" %MagickClipPathImage)
    %magick-status
  "Clips along the named paths from the 8BIM profile, if present. Later operations take effect inside the path. Id may be a number if preceded with #, to work on a numbered path, e.g., \"#1\" to use the first path.

The format of the MagickClipPathImage method is:
  unsigned int MagickClipPathImage\( MagickWand *wand, const char *pathname,
                                  const unsigned int inside );
wand:
  The magick wand.
pathname:
  name of clipping path resource. If name is preceded by #, use
clipping path numbered by name.
inside:
  if non-zero, later operations take effect inside clipping path.
Otherwise later operations take effect outside clipping path.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (pathname :string)
  (inside :unsigned-int))

(defcfun ("MagickCoalesceImages" %MagickCoalesceImages)
    %MagickWand
  "Composites a set of images while respecting any page offsets and disposal methods. GIF, MIFF, and MNG animation sequences typically start with an image background and each subsequent image varies in size and offset. MagickCoalesceImages\() returns a new sequence where each image in the sequence is the same size as the first and composited with the next image in the sequence.

The format of the MagickCoalesceImages method is:
  MagickWand *MagickCoalesceImages\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickColorFloodfillImage" %MagickColorFloodfillImage)
    %magick-status
  "Changes the color value of any pixel that matches target and is an immediate neighbor. If the method FillToBorderMethod is specified, the color value is changed for any neighbor pixel that does not match the bordercolor member of image.

The format of the MagickColorFloodfillImage method is:
  unsigned int MagickColorFloodfillImage\( MagickWand *wand, const PixelWand *fill,
                                        const double fuzz, const PixelWand *bordercolor,
                                        const long x, const long y );
wand:
  The magick wand.
fill:
  The floodfill color pixel wand.
fuzz:
  By default target must match a particular pixel color
exactly.  However, in many cases two colors may differ by a small amount.
The fuzz member of image defines how much tolerance is acceptable to
consider two colors as the same.  For example, set fuzz to 10 and the
color red at intensities of 100 and 102 respectively are now interpreted
as the same color for the purposes of the floodfill.
bordercolor:
  The border color pixel wand.
x,y:
  The starting location of the operation.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (fill %PixelWand)
  (fuzz :double)
  (bordercolor %PixelWand)
  (x :long)
  (y :long))

(defcfun ("MagickColorizeImage" %MagickColorizeImage)
    %magick-status
  "Blends the fill color with each pixel in the image.

The format of the MagickColorizeImage method is:
  unsigned int MagickColorizeImage\( MagickWand *wand, const PixelWand *colorize,
                                  const PixelWand *opacity );
wand:
  The magick wand.
colorize:
  The colorize pixel wand.
opacity:
  The opacity pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (colorize %PixelWand)
  (opacity %PixelWand))

(defcfun ("MagickCommentImage" %MagickCommentImage)
    %magick-status
  "Adds a comment to your image.

The format of the MagickCommentImage method is:
  unsigned int MagickCommentImage\( MagickWand *wand, const char *comment );

A description of each parameter follows:
wand:
  The magick wand.
comment:
  The image comment.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (comment :string))

(defcfun ("MagickCompareImageChannels" %MagickCompareImageChannels)
    %MagickWand
  "Compares one or more image channels and returns the specified distortion metric.

The format of the MagickCompareImageChannels method is:
  MagickWand *MagickCompareImageChannels\( MagickWand *wand, const MagickWand *reference,
                                        const ChannelType channel, const MetricType metric,
                                        double *distortion );
wand:
  The magick wand.
reference:
  The reference wand.
channel:
  The channel.
metric:
  The metric.
distortion:
  The computed distortion between the images.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (reference %MagickWand)
  (channel %ChannelType)
  (metric %MetricType)
  (distortion :pointer))

(defcfun ("MagickCompareImages" %MagickCompareImages)
    %MagickWand
  "Compares one or more images and returns the specified distortion metric.

The format of the MagickCompareImages method is:
  MagickWand *MagickCompareImages\( MagickWand *wand, const MagickWand *reference,
                                 const MetricType metric, double *distortion );
wand:
  The magick wand.
reference:
  The reference wand.
metric:
  The metric.
distortion:
  The computed distortion between the images.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (reference %MagickWand)
  (metric %MetricType)
  (distortion :pointer))

(defcfun ("MagickCompositeImage" %MagickCompositeImage)
    %magick-status
  "Composite one image onto another at the specified offset.

The format of the MagickCompositeImage method is:
  unsigned int MagickCompositeImage\( MagickWand *wand, const MagickWand *composite_wand,
                                   const CompositeOperator compose, const long x,
                                   const long y );
wand:
  The magick wand.
composite_image:
  The composite image.
compose:
  This operator affects how the composite is applied to the
image.  The default is Over.  Choose from these operators:

OverCompositeOp       InCompositeOp         OutCompositeOP
AtopCompositeOP       XorCompositeOP        PlusCompositeOP
MinusCompositeOP      AddCompositeOP        SubtractCompositeOP
DifferenceCompositeOP BumpmapCompositeOP    CopyCompositeOP
DisplaceCompositeOP
x_offset:
  The column offset of the composited image.
y_offset:
  The row offset of the composited image.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (composite_wand %MagickWand)
  (compose %CompositeOperator)
  (x :long)
  (y :long))

(defcfun ("MagickContrastImage" %MagickContrastImage)
    %magick-status
  "Enhances the intensity differences between the lighter and darker elements of the image. Set sharpen to a value other than 0 to increase the image contrast otherwise the contrast is reduced.

The format of the MagickContrastImage method is:
  unsigned int MagickContrastImage\( MagickWand *wand, const unsigned int sharpen );
wand:
  The magick wand.
sharpen:
  Increase or decrease image contrast.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (sharpen :unsigned-int))

(defcfun ("MagickConvolveImage" %MagickConvolveImage)
    %magick-status
  "Applies a custom convolution kernel to the image.

The format of the MagickConvolveImage method is:
  unsigned int MagickConvolveImage\( MagickWand *wand, const unsigned long order,
                                  const double *kernel );
wand:
  The magick wand.
order:
  The number of columns and rows in the filter kernel.
kernel:
  An array of doubles representing the convolution kernel.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (order :unsigned-long)
  (kernel :pointer))

(defcfun ("MagickCropImage" %MagickCropImage)
    %magick-status
  "Extracts a region of the image.

The format of the MagickCropImage method is:
  unsigned int MagickCropImage\( MagickWand *wand, const unsigned long width,
                              const unsigned long height, const long x, const long y );
wand:
  The magick wand.
width:
  The region width.
height:
  The region height.
x:
  The region x offset.
y:
  The region y offset.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (width :unsigned-long)
  (height :unsigned-long)
  (x :long)
  (y :long))

(defcfun ("MagickCycleColormapImage" %MagickCycleColormapImage)
    %magick-status
  "Displaces an image's colormap by a given number of positions. If you cycle the colormap a number of times you can produce a psychodelic effect.

The format of the MagickCycleColormapImage method is:
  unsigned int MagickCycleColormapImage\( MagickWand *wand, const long displace );
wand:
  The magick wand.
pixel_wand:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (displace :long))

(defcfun ("MagickDeconstructImages" %MagickDeconstructImages)
    %MagickWand
  "Compares each image with the next in a sequence and returns the maximum bounding region of any pixel differences it discovers.

The format of the MagickDeconstructImages method is:
  MagickWand *MagickDeconstructImages\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickDescribeImage" %MagickDescribeImage)
    :string
  "Describes an image by formatting its attributes to an allocated string which must be freed by the user. Attributes include the image width, height, size, and others. The string is similar to the output of 'identify -verbose'.

The format of the MagickDescribeImage method is:
  const char *MagickDescribeImage\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickDespeckleImage" %MagickDespeckleImage)
    %magick-status
  "Reduces the speckle noise in an image while perserving the edges of the original image.

The format of the MagickDespeckleImage method is:
  unsigned int MagickDespeckleImage\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickDisplayImage" %MagickDisplayImage)
    %magick-status
  "Displays an image.

The format of the MagickDisplayImage method is:
  unsigned int MagickDisplayImage\( MagickWand *wand, const char *server_name );

A description of each parameter follows:
wand:
  The magick wand.
server_name:
  The X server name.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (server_name :string))

(defcfun ("MagickDisplayImages" %MagickDisplayImages)
    %magick-status
  "Displays an image or image sequence.

The format of the MagickDisplayImages method is:
  unsigned int MagickDisplayImages\( MagickWand *wand, const char *server_name );
wand:
  The magick wand.
server_name:
  The X server name.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (server_name :string))

(defcfun ("MagickDrawImage" %MagickDrawImage)
    %magick-status
  "Draws vectors on the image as described by DrawingWand.

The format of the MagickDrawImage method is:
  unsigned int MagickDrawImage\( MagickWand *wand, const DrawingWand *drawing_wand );
wand:
  The magick wand.
drawing_wand:
  The draw wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (drawing_wand %DrawingWand))

(defcfun ("MagickEdgeImage" %MagickEdgeImage)
    %magick-status
  "Enhance edges within the image with a convolution filter of the given radius. Use a radius of 0 and Edge\() selects a suitable radius for you.

The format of the MagickEdgeImage method is:
  unsigned int MagickEdgeImage\( MagickWand *wand, const double radius );

A description of each parameter follows:
wand:
  The magick wand.
radius:
  the radius of the pixel neighborhood.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (radius :double))

(defcfun ("MagickEmbossImage" %MagickEmbossImage)
    %magick-status
  "Returns a grayscale image with a three-dimensional effect. We convolve the image with a Gaussian operator of the given radius and standard deviation \(sigma). For reasonable results, radius should be larger than sigma. Use a radius of 0 and Emboss\() selects a suitable radius for you.

The format of the MagickEmbossImage method is:
  unsigned int MagickEmbossImage\( MagickWand *wand, const double radius, const double sigma );
wand:
  The magick wand.
radius:
  The radius of the Gaussian, in pixels, not counting the center
pixel.
sigma:
  The standard deviation of the Gaussian, in pixels.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (radius :double)
  (sigma :double))

(defcfun ("MagickEnhanceImage" %MagickEnhanceImage)
    %magick-status
  "Applies a digital filter that improves the quality of a noisy image.

The format of the MagickEnhanceImage method is:
  unsigned int MagickEnhanceImage\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickEqualizeImage" %MagickEqualizeImage)
    %magick-status
  "Equalizes the image histogram.

The format of the MagickEqualizeImage method is:
  unsigned int MagickEqualizeImage\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickExtentImage" %MagickExtentImage)
    %magick-status
  "Use MagickExtentImage\() to change the image dimensions as specified by geometry width and height. The existing image content is composited at the position specified by geometry x and y using the image compose method. Existing image content which falls outside the bounds of the new image dimensions is discarded.

The format of the MagickExtentImage method is:
  unsigned int MagickExtentImage\( MagickWand *wand, const size_t width, const size_t height,
                                const ssize_t x, const ssize_t y );
wand:
  The magick wand.
width:
  New image width
height:
  New image height
x, y:
  Top left composition coordinate to place existing image content
on the new image.

Since GraphicsMagick v1.3.14"
  (wand %MagickWand)
  (width %size_t)
  (height %size_t)
  (x %ssize_t)
  (y %ssize_t))

(defcfun ("MagickFlattenImages" %MagickFlattenImages)
    %MagickWand
  "Merges a sequence of images. This is useful for combining Photoshop layers into a single image.

The format of the MagickFlattenImages method is:
  MagickWand *MagickFlattenImages\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickFlipImage" %MagickFlipImage)
    %magick-status
  "Creates a vertical mirror image by reflecting the pixels around the central x-axis\(swap up/down).

The format of the MagickFlipImage method is:
  unsigned int MagickFlipImage\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickFlopImage" %MagickFlopImage)
    %magick-status
  "Creates a horizontal mirror image by reflecting the pixels around the central y-axis\(swap left/right).

The format of the MagickFlopImage method is:
  unsigned int MagickFlopImage\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickFrameImage" %MagickFrameImage)
    %magick-status
  "Adds a simulated three-dimensional border around the image. The width and height specify the border width of the vertical and horizontal sides of the frame. The inner and outer bevels indicate the width of the inner and outer shadows of the frame.

The format of the MagickFrameImage method is:
  unsigned int MagickFrameImage\( MagickWand *wand, const PixelWand *matte_color,
                               const unsigned long width, const unsigned long height,
                               const long inner_bevel, const long outer_bevel );
wand:
  The magick wand.
matte_color:
  The frame color pixel wand.
width:
  The border width.
height:
  The border height.
inner_bevel:
  The inner bevel width.
outer_bevel:
  The outer bevel width.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (matte_color %PixelWand)
  (width :unsigned-long)
  (height :unsigned-long)
  (inner_bevel :long)
  (outer_bevel :long))

(defcfun ("MagickFxImage" %MagickFxImage)
    %MagickWand
  "Evaluate expression for each pixel in the image.

The format of the MagickFxImage method is:
  MagickWand *MagickFxImage\( MagickWand *wand, const char *expression );

A description of each parameter follows:
wand:
  The magick wand.
expression:
  The expression.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (expression :string))

(defcfun ("MagickFxImageChannel" %MagickFxImageChannel)
    %MagickWand
  "Evaluate expression for each pixel in the specified channel.

The format of the MagickFxImageChannel method is:
  MagickWand *MagickFxImageChannel\( MagickWand *wand, const ChannelType channel,
                                  const char *expression );
wand:
  The magick wand.
channel:
  Identify which channel to level: RedChannel, GreenChannel,
BlueChannel, OpacityChannel, CyanChannel, MagentaChannel, YellowChannel,
BlackChannel.
expression:
  The expression.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (channel %ChannelType)
  (expression :string))

(defcfun ("MagickGammaImage" %MagickGammaImage)
    %magick-status
  "Use MagickGammaImage\() to gamma-correct an image. The same image viewed on different devices will have perceptual differences in the way the image's intensities are represented on the screen. Specify individual gamma levels for the red, green, and blue channels, or adjust all three with the gamma parameter. Values typically range from 0.8 to 2.3.

You can also reduce the influence of a particular channel with a gamma
value of 0.

The format of the MagickGammaImage method is:
  unsigned int MagickGammaImage\( MagickWand *wand, const double gamma );

A description of each parameter follows:
wand:
  The magick wand.
gamme:
  Define the level of gamma correction.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (gamma :double))

(defcfun ("MagickGammaImageChannel" %MagickGammaImageChannel)
    %magick-status
  "Use MagickGammaImageChannel\() to gamma-correct a particular image channel. The same image viewed on different devices will have perceptual differences in the way the image's intensities are represented on the screen. Specify individual gamma levels for the red, green, and blue channels, or adjust all three with the gamma parameter. Values typically range from 0.8 to 2.3.

You can also reduce the influence of a particular channel with a gamma
value of 0.

The format of the MagickGammaImageChannel method is:
  unsigned int MagickGammaImageChannel\( MagickWand *wand, const ChannelType channel,
                                      const double gamma );
wand:
  The magick wand.
channel:
  The channel.
level:
  Define the level of gamma correction.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (channel %ChannelType)
  (gamma :double))

(defcfun ("MagickGetConfigureInfo" %MagickGetConfigureInfo)
    :string
  "Returns ImageMagick configure attributes such as NAME, VERSION, LIB_VERSION, COPYRIGHT, etc.

The format of the MagickGetConfigureInfo\() method is:
  char *MagickGetConfigureInfo\( MagickWand *wand, const char *name );

A description of each parameter follows:
wand:
  The magick wand.
name:
  Return the attribute associated with this name.

WARN: GraphicsMagick return NULL whatever you passed. Do NOT use this API. 
        More detail could be found in source code.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (name :string))

(defcfun ("MagickGetCopyright" %MagickGetCopyright)
    :string
  "Returns the ImageMagick API copyright as a string.

The format of the MagickGetCopyright method is:
  const char *MagickGetCopyright\( void );

Since GraphicsMagick v1.1.0")

(defcfun ("MagickGetException" %MagickGetException)
    :string
  "Returns the severity, reason, and description of any error that occurs when using other methods in this API.

The format of the MagickGetException method is:
  char *MagickGetException\( const MagickWand *wand, ExceptionType *severity );

A description of each parameter follows:
wand:
  The magick wand.
severity:
  The severity of the error is returned here.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (severity :pointer)) ;; ExceptionType *severity

(defcfun ("MagickGetFilename" %MagickGetFilename)
    :string
  "Returns the filename associated with an image sequence.

The format of the MagickGetFilename method is:
  const char *MagickGetFilename\( const MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetHomeURL" %MagickGetHomeURL)
    :string
  "Returns the ImageMagick home URL.

The format of the MagickGetHomeURL method is:
  const char *MagickGetHomeURL\( void );

Since GraphicsMagick v1.1.0")

(defcfun ("MagickGetImage" %MagickGetImage)
    %MagickWand
  "Clones the image at the current image index.

The format of the MagickGetImage method is:
  MagickWand *MagickGetImage\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageAttribute" %MagickGetImageAttribute)
    :string
  "MagickGetImageAttribute returns an image attribute as a string

The format of the MagickGetImageAttribute method is:
  char *MagickGetImageAttribute\( MagickWand *wand, const char *name );

A description of each parameter follows:
wand:
  The magick wand.
name:
  The name of the attribute

Since GraphicsMagick v1.3.8"
  (wand %MagickWand)
  (name :string))

(defcfun ("MagickGetImageBackgroundColor" %MagickGetImageBackgroundColor)
    %magick-status
  "Returns the image background color.

The format of the MagickGetImageBackgroundColor method is:
  unsigned int MagickGetImageBackgroundColor\( MagickWand *wand, PixelWand *background_color );
wand:
  The magick wand.
background_color:
  Return the background color.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (background_color %PixelWand))

(defcfun ("MagickGetImageBluePrimary" %MagickGetImageBluePrimary)
    %magick-status
  "Returns the chromaticy blue primary point for the image.

The format of the MagickGetImageBluePrimary method is:
  unsigned int MagickGetImageBluePrimary\( MagickWand *wand, double *x, double *y );
wand:
  The magick wand.
x:
  The chromaticity blue primary x-point.
y:
  The chromaticity blue primary y-point.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (x :pointer)
  (y :pointer))

(defcfun ("MagickGetImageBorderColor" %MagickGetImageBorderColor)
    %magick-status
  "Returns the image border color.

The format of the MagickGetImageBorderColor method is:
  unsigned int MagickGetImageBorderColor\( MagickWand *wand, PixelWand *border_color );
wand:
  The magick wand.
border_color:
  Return the border color.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (border_color %PixelWand))

(defcfun ("MagickGetImageBoundingBox" %MagickGetImageBoundingBox)
    %magick-status
  "Obtains the crop bounding box required to remove a solid-color border from the image. Color quantums which differ less than the fuzz setting are considered to be the same. If a border is not detected, then the the original image dimensions are returned. The crop bounding box estimation uses the same algorithm as MagickTrimImage\().

The format of the MagickGetImageBoundingBox method is:
  unsigned int MagickGetImageBoundingBox\( MagickWand *wand, const double fuzz,
                                        unsigned long *width, unsigned long *height,
                                        long *x, long *y );
wand:
  The magick wand.
fuzz:
  Color comparison fuzz factor.  Use 0.0 for exact match.
width:
  The crop width
height:
  The crop height
x:
  The crop x offset
y:
  The crop y offset

Since GraphicsMagick v1.3.8"
  (wand %MagickWand)
  (fuzz :double)
  (width :pointer)
  (height :pointer)
  (x :pointer)
  (y :pointer))

(defcfun ("MagickGetImageChannelDepth" %MagickGetImageChannelDepth)
    :unsigned-long
  "Gets the depth for a particular image channel.

The format of the MagickGetImageChannelDepth method is:
  unsigned long MagickGetImageChannelDepth\( MagickWand *wand, const ChannelType channel );
wand:
  The magick wand.
channel:
  Identify which channel to extract: RedChannel, GreenChannel,
BlueChannel, OpacityChannel, CyanChannel, MagentaChannel, YellowChannel,
BlackChannel.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (channel %ChannelType))

(defcfun ("MagickGetImageChannelExtrema" %MagickGetImageChannelExtrema)
    %magick-status
  "Gets the extrema for one or more image channels.

The format of the MagickGetImageChannelExtrema method is:
  unsigned int MagickGetImageChannelExtrema\( MagickWand *wand, const ChannelType channel,
                                           unsigned long *minima, unsigned long *maxima );
wand:
  The magick wand.
channel:
  Identify which channel to extract: RedChannel, GreenChannel,
BlueChannel, OpacityChannel, CyanChannel, MagentaChannel, YellowChannel,
or BlackChannel.
minima:
  The minimum pixel value for the specified channel\(s).
maxima:
  The maximum pixel value for the specified channel\(s).

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (channel %ChannelType)
  (minima :pointer)
  (maxima :pointer))

(defcfun ("MagickGetImageChannelMean" %MagickGetImageChannelMean)
    %magick-status
  "Gets the mean and standard deviation of one or more image channels.

The format of the MagickGetImageChannelMean method is:
  unsigned int MagickGetImageChannelMean\( MagickWand *wand, const ChannelType channel,
                                        double *mean, double *standard_deviation );
wand:
  The magick wand.
channel:
  Identify which channel to extract: RedChannel, GreenChannel,
BlueChannel, OpacityChannel, CyanChannel, MagentaChannel, YellowChannel,
or BlackChannel.
mean:
  The mean pixel value for the specified channel\(s).
standard_deviation:
  The standard deviation for the specified channel\(s).

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (channel %ChannelType)
  (mean :pointer)
  (standard_deviation :pointer))

(defcfun ("MagickGetImageColormapColor" %MagickGetImageColormapColor)
    %magick-status
  "Returns the color of the specified colormap index.

The format of the MagickGetImageColormapColor method is:
  unsigned int MagickGetImageColormapColor\( MagickWand *wand, const unsigned long index,
                                          PixelWand *color );
wand:
  The magick wand.
index:
  The offset into the image colormap.
color:
  Return the colormap color in this wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (index :unsigned-long)
  (color %PixelWand))

(defcfun ("MagickGetImageColors" %MagickGetImageColors)
    :unsigned-long
  "Gets the number of unique colors in the image.

The format of the MagickGetImageColors method is:
  unsigned long MagickGetImageColors\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageColorspace" %MagickGetImageColorspace)
    %ColorspaceType
  "Gets the image colorspace.

The format of the MagickGetImageColorspace method is:
  ColorspaceType MagickGetImageColorspace\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageCompose" %MagickGetImageCompose)
    %CompositeOperator
  "Returns the composite operator associated with the image.

The format of the MagickGetImageCompose method is:
  CompositeOperator MagickGetImageCompose\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageCompression" %MagickGetImageCompression)
    %CompressionType
  "Gets the image compression.

The format of the MagickGetImageCompression method is:
  CompressionType MagickGetImageCompression\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageDelay" %MagickGetImageDelay)
    :unsigned-long
  "Gets the image delay.

The format of the MagickGetImageDelay method is:
  unsigned long MagickGetImageDelay\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageDepth" %MagickGetImageDepth)
    :unsigned-long
  "Gets the image depth.

The format of the MagickGetImageDepth method is:
  unsigned long MagickGetImageDepth\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageExtrema" %MagickGetImageExtrema)
    %magick-status
  "Gets the extrema for the image.

The format of the MagickGetImageExtrema method is:
  unsigned int MagickGetImageExtrema\( MagickWand *wand, unsigned long *min,
                                    unsigned long *max );
wand:
  The magick wand.
min:
  The minimum pixel value for the specified channel\(s).
max:
  The maximum pixel value for the specified channel\(s).

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (min :pointer)
  (max :pointer))

(defcfun ("MagickGetImageDispose" %MagickGetImageDispose)
    %DisposeType
  "Gets the image disposal method.

The format of the MagickGetImageDispose method is:
  DisposeType MagickGetImageDispose\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageFilename" %MagickGetImageFilename)
    :char
  "Returns the filename of a particular image in a sequence.

The format of the MagickGetImageFilename method is:
  const char MagickGetImageFilename\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageFormat" %MagickGetImageFormat)
    :string
  "Returns the format of a particular image in a sequence.

The format of the MagickGetImageFormat method is:
  char * MagickGetImageFormat\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageFuzz" %MagickGetImageFuzz)
    :double
  "Returns the color comparison fuzz factor. Colors closer than the fuzz factor are considered to be the same when comparing colors. Note that some other functions such as MagickColorFloodfillImage\() implicitly set this value.

The format of the MagickGetImageFuzz method is:
  double MagickGetImageFuzz\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.3.8"
  (wand %MagickWand))

(defcfun ("MagickGetImageGamma" %MagickGetImageGamma)
    :double
  "Gets the image gamma.

The format of the MagickGetImageGamma method is:
  double MagickGetImageGamma\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageGeometry" %MagickGetImageGeometry)
    :string
  "Gets the image geometry string.  NULL is returned if the image does not contain a geometry string.

The format of the MagickGetImageGeometry method is:
  const char *MagickGetImageGeometry\( MagickWand *wand )

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.3.20"
  (wand %MagickWand))

(defcfun ("MagickGetImageGravity" %MagickGetImageGravity)
    %GravityType
  "Gets the image gravity.

The format of the MagickGetImageGravity method is:
  GravityType MagickGetImageGravity\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.3.22"
  (wand %MagickWand))

(defcfun ("MagickGetImageGreenPrimary" %MagickGetImageGreenPrimary)
    %magick-status
  "Returns the chromaticy green primary point.

The format of the MagickGetImageGreenPrimary method is:
  unsigned int MagickGetImageGreenPrimary\( MagickWand *wand, double *x, double *y );
wand:
  The magick wand.
x:
  The chromaticity green primary x-point.
y:
  The chromaticity green primary y-point.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (x :pointer)
  (y :pointer))

(defcfun ("MagickGetImageHeight" %MagickGetImageHeight)
    :unsigned-long
  "Returns the image height.

The format of the MagickGetImageHeight method is:
  unsigned long MagickGetImageHeight\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageHistogram" %MagickGetImageHistogram)
    %PixelWand
  "Returns the image histogram as an array of PixelWand wands.

The format of the MagickGetImageHistogram method is:
  PixelWand *MagickGetImageHistogram\( MagickWand *wand, unsigned long *number_colors );
wand:
  The magick wand.
number_colors:
  The number of unique colors in the image and the number
of pixel wands returned.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (number_colors :pointer))

(defcfun ("MagickGetImageIndex" %MagickGetImageIndex)
    :long
  "Returns the index of the current image.

The format of the MagickGetImageIndex method is:
  long MagickGetImageIndex\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageInterlaceScheme" %MagickGetImageInterlaceScheme)
    %InterlaceType
  "Gets the image interlace scheme.

The format of the MagickGetImageInterlaceScheme method is:
  InterlaceType MagickGetImageInterlaceScheme\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageIterations" %MagickGetImageIterations)
    :unsigned-long
  "Gets the image iterations.

The format of the MagickGetImageIterations method is:
  unsigned long MagickGetImageIterations\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageMatte" %MagickGetImageMatte)
    %MagickBool
  "Gets the image matte flag.  The flag is MagickTrue if the image supports an opacity \(inverse of transparency) channel.

The format of the MagickGetImageMatte method is:
  MagickBool MagickGetImageMatte\( MagickWand *wand )

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.3.20"
  (wand %MagickWand))

(defcfun ("MagickGetImageMatteColor" %MagickGetImageMatteColor)
    %magick-status
  "Returns the image matte color.

The format of the MagickGetImageMatteColor method is:
  unsigned int MagickGetImageMatteColor\( MagickWand *wand, PixelWand *matte_color );
wand:
  The magick wand.
matte_color:
  Return the matte color.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (matte_color %PixelWand))

(defcfun ("MagickGetImageOrientation" %MagickGetImageOrientation)
    %OrientationType
  "Gets the image orientation type. May be one of:

  UndefinedOrientation Image orientation not specified or error.
  TopLeftOrientation Left to right and Top to bottom.
  TopRightOrientation Right to left and Top to bottom.
  BottomRightOrientation Right to left and Bottom to top.
  BottomLeftOrientation Left to right and Bottom to top.
  LeftTopOrientation Top to bottom and Left to right.
  RightTopOrientation Top to bottom and Right to left.
  RightBottomOrientation Bottom to top and Right to left.
  LeftBottomOrientation Bottom to top and Left to right.

The format of the MagickGetImageOrientation method is:
  OrientationType MagickGetImageOrientation\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.3.26"
  (wand %MagickWand))

(defcfun ("MagickGetImagePage" %MagickGetImagePage)
    %magick-status
  "Retrieves the image page size and offset used when placing \(e.g. compositing) the image.

The format of the MagickGetImagePage method is:
  MagickGetImagePage\( MagickWand *wand, unsigned long *width, unsigned long *height, long *x,
                    long *y );
wand:
  The magick wand.
width, height:
  The region size.
x, y:
  Offset \(from top left) on base canvas image on
which to composite image data.

Since GraphicsMagick v1.3.18"
  (wand %MagickWand)
  (width :pointer)
  (height :pointer)
  (x :pointer)
  (y :pointer))

(defcfun ("MagickGetImagePixels" %MagickGetImagePixels)
    %magick-status
  "Extracts pixel data from an image and returns it to you. The method returns False on success otherwise True if an error is encountered. The data is returned as char, short int, int, long, float, or double in the order specified by map.

Suppose you want to extract the first scanline of a 640x480 image as
character data in red-green-blue order:

MagickGetImagePixels\(wand,0,0,640,1,\"RGB\",CharPixel,pixels);

The format of the MagickGetImagePixels method is:
  unsigned int MagickGetImagePixels\( MagickWand *wand, const long x_offset, const long y_offset,
                                   const unsigned long columns, const unsigned long rows,
                                   const char *map, const StorageType storage,
                                   unsigned char *pixels );
wand:
  The magick wand.
x_offset, y_offset, columns, rows:
  These values define the perimeter
of a region of pixels you want to extract.
map:
  This string reflects the expected ordering of the pixel array.
It can be any combination or order of R = red, G = green, B = blue,
A = alpha, C = cyan, Y = yellow, M = magenta, K = black, or
I = intensity \(for grayscale).
storage:
  Define the data type of the pixels.  Float and double types are
expected to be normalized [0..1] otherwise [0..MaxRGB].  Choose from
these types: CharPixel, ShortPixel, IntegerPixel, LongPixel, FloatPixel,
or DoublePixel.
pixels:
  This array of values contain the pixel components as defined by
map and type.  You must preallocate this array where the expected
length varies depending on the values of width, height, map, and type.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (x_offset :long)
  (y_offset :long)
  (columns :unsigned-long)
  (rows :unsigned-long)
  (map :string)
  (storage %StorageType)
  (pixels :pointer))

(defcfun ("MagickGetImageProfile" %MagickGetImageProfile)
    :pointer
  "Returns the named image profile.

The format of the MagickGetImageProfile method is:
  unsigned char *MagickGetImageProfile\( MagickWand *wand, const char *name,
                                      unsigned long *length );
wand:
  The magick wand.
name:
  Name of profile to return: ICC, IPTC, or generic profile.
length:
  The length of the profile.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (name :string)
  (length :pointer))

(defcfun ("MagickGetImageRedPrimary" %MagickGetImageRedPrimary)
    %magick-status
  "Returns the chromaticy red primary point.

The format of the MagickGetImageRedPrimary method is:
  unsigned int MagickGetImageRedPrimary\( MagickWand *wand, double *x, double *y );
wand:
  The magick wand.
x:
  The chromaticity red primary x-point.
y:
  The chromaticity red primary y-point.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (x :pointer)
  (y :pointer))

(defcfun ("MagickGetImageRenderingIntent" %MagickGetImageRenderingIntent)
    %RenderingIntent
  "Gets the image rendering intent.

The format of the MagickGetImageRenderingIntent method is:
  RenderingIntent MagickGetImageRenderingIntent\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageResolution" %MagickGetImageResolution)
    %magick-status
  "Gets the image X & Y resolution.

The format of the  MagickGetImageResolution method is:
  unsigned int MagickGetImageResolution\( MagickWand *wand, double *x, double *y );
wand:
  The magick wand.
x:
  The image x-resolution.
y:
  The image y-resolution.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (x :pointer)
  (y :pointer))

(defcfun ("MagickGetImageScene" %MagickGetImageScene)
    :unsigned-long
  "Gets the image scene.

The format of the MagickGetImageScene method is:
  unsigned long MagickGetImageScene\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageSignature" %MagickGetImageSignature)
    :char
  "Generates an SHA-256 message digest for the image pixel stream.

The format of the MagickGetImageSignature method is:
  const char MagickGetImageSignature\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageSize" %MagickGetImageSize)
    %MagickSizeType
  "Returns the image size.

The format of the MagickGetImageSize method is:
  MagickSizeType MagickGetImageSize\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageType" %MagickGetImageType)
    %ImageType
  "Gets the image type.

The format of the MagickGetImageType method is:
  ImageType MagickGetImageType\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageSavedType" %MagickGetImageSavedType)
    %ImageType
  "Gets the image type that will be used when the image is saved. This may be different to the current image type, returned by MagickGetImageType\().

The format of the MagickGetImageSavedType method is:
  ImageType MagickGetImageSavedType\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.3.13"
  (wand %MagickWand))

(defcfun ("MagickGetImageUnits" %MagickGetImageUnits)
    %ResolutionType
  "Gets the image units of resolution.

The format of the MagickGetImageUnits method is:
  ResolutionType MagickGetImageUnits\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageVirtualPixelMethod" %MagickGetImageVirtualPixelMethod)
    %VirtualPixelMethod
  "Returns the virtual pixel method for the sepcified image.

The format of the MagickGetImageVirtualPixelMethod method is:
  VirtualPixelMethod MagickGetImageVirtualPixelMethod\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetImageWhitePoint" %MagickGetImageWhitePoint)
    %magick-status
  "Returns the chromaticy white point.

The format of the MagickGetImageWhitePoint method is:
  unsigned int MagickGetImageWhitePoint\( MagickWand *wand, double *x, double *y );
wand:
  The magick wand.
x:
  The chromaticity white x-point.
y:
  The chromaticity white y-point.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (x :pointer)
  (y :pointer))

(defcfun ("MagickGetImageWidth" %MagickGetImageWidth)
    :unsigned-long
  "Returns the image width.

The format of the MagickGetImageWidth method is:
  unsigned long MagickGetImageWidth\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetNumberImages" %MagickGetNumberImages)
    :unsigned-long
  "Returns the number of images associated with a magick wand.

The format of the MagickGetNumberImages method is:
  unsigned long MagickGetNumberImages\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickGetPackageName" %MagickGetPackageName)
    :string
  "Returns the ImageMagick package name.

The format of the MagickGetPackageName method is:
  const char *MagickGetPackageName\( void );

Since GraphicsMagick v1.1.0")

(defcfun ("MagickGetQuantumDepth" %MagickGetQuantumDepth)
    :string
  "Returns the ImageMagick quantum depth.

The format of the MagickGetQuantumDepth method is:
  const char *MagickGetQuantumDepth\( unsigned long *depth );

A description of each parameter follows:
depth:
  The quantum depth is returned as a number.

Since GraphicsMagick v1.1.0"
  (depth :pointer))

(defcfun ("MagickGetReleaseDate" %MagickGetReleaseDate)
    :string
  "Returns the ImageMagick release date.

The format of the MagickGetReleaseDate method is:
  const char *MagickGetReleaseDate\( void );

Since GraphicsMagick v1.1.0")

(defcfun ("MagickGetResourceLimit" %MagickGetResourceLimit)
    :unsigned-long
  "Returns the the specified resource in megabytes.

The format of the MagickGetResourceLimit method is:
  unsigned long MagickGetResourceLimit\( const ResourceType type );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (type %ResourceType))

(defcfun ("MagickGetSamplingFactors" %MagickGetSamplingFactors)
    :double
  "Gets the horizontal and vertical sampling factor.

The format of the MagickGetSamplingFactors method is:
  double *MagickGetSamplingFactors\( MagickWand *wand, unsigned long *number_factors );
wand:
  The magick wand.
number_factors:
  The number of factors in the returned array.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (number_factors :pointer))

(defcfun ("MagickGetSize" %MagickGetSize)
    %magick-status
  "Returns the size associated with the magick wand.

The format of the MagickGetSize method is:
  unsigned int MagickGetSize\( const MagickWand *wand, unsigned long *columns,
                            unsigned long *rows );
wand:
  The magick wand.
columns:
  The width in pixels.
height:
  The height in pixels.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (columns :pointer)
  (rows :pointer))

(defcfun ("MagickGetVersion" %MagickGetVersion)
    :string
  "Returns the ImageMagick API version as a string and as a number.

The format of the MagickGetVersion method is:
  const char *MagickGetVersion\( unsigned long *version );

A description of each parameter follows:
version:
  The ImageMagick version is returned as a number.

Since GraphicsMagick v1.1.0"
  (version :pointer))

(defcfun ("MagickHaldClutImage" %MagickHaldClutImage)
    %MagickPassFail
  "The MagickHaldClutImage\() method apply a color lookup table \(Hald CLUT) to the image. The fundamental principle of the Hald CLUT algorithm is that application of an identity CLUT causes no change to the input image, but an identity CLUT image which has had its colors transformed in some way \(e.g. in Adobe Photoshop) may be used to implement an identical transform on any other image.

The minimum CLUT level is 2, and the maximum depends on available memory
\(largest successfully tested is 24).  A CLUT image is required to have equal
width and height. A CLUT of level 8 is an image of dimension 512x512, a CLUT
of level 16 is an image of dimension 4096x4096.  Interpolation is used so
extremely large CLUT images are not required.

GraphicsMagick provides an 'identity' coder which may be used to generate
identity HLUTs.  For example, reading from \"identity:8\" creates an identity
CLUT of order 8.

The Hald CLUT algorithm has been developed by Eskil Steenberg as described
at http://www.quelsolaar.com/technology/clut.html, and was adapted for
GraphicsMagick by Clément Follet with support from Cédric Lejeune of
Workflowers.

The format of the HaldClutImage method is:
  MagickPassFail MagickHaldClutImage\( MagickWand *wand, const MagickWand *clut_wand );

A description of each parameter follows:
wand:
  The image wand.
clut_wand:
  The color lookup table image wand

Since GraphicsMagick v1.3.8"
  (wand %MagickWand)
  (clut_wand %MagickWand))

(defcfun ("MagickHasNextImage" %MagickHasNextImage)
    :boolean
  "Returns True if the wand has more images when traversing the list in the forward direction

The format of the MagickHasNextImage method is:
  unsigned int MagickHasNextImage\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickHasPreviousImage" %MagickHasPreviousImage)
    %magick-status
  "Returns True if the wand has more images when traversing the list in the reverse direction

The format of the MagickHasPreviousImage method is:
  unsigned int MagickHasPreviousImage\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickImplodeImage" %MagickImplodeImage)
    %magick-status
  "Creates a new image that is a copy of an existing one with the image pixels \"implode\" by the specified percentage. It allocates the memory necessary for the new Image structure and returns a pointer to the new image.

The format of the MagickImplodeImage method is:
  unsigned int MagickImplodeImage\( MagickWand *wand, const double radius );

A description of each parameter follows:
wand:
  The magick wand.
amount:
  Define the extent of the implosion.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (radius :double))

(defcfun ("MagickLabelImage" %MagickLabelImage)
    %magick-status
  "Adds a label to your image.

The format of the MagickLabelImage method is:
  unsigned int MagickLabelImage\( MagickWand *wand, const char *label );

A description of each parameter follows:
wand:
  The magick wand.
label:
  The image label.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (label :string))

(defcfun ("MagickLevelImage" %MagickLevelImage)
    %magick-status
  "Adjusts the levels of an image by scaling the colors falling between specified white and black points to the full available quantum range. The parameters provided represent the black, mid, and white points. The black point specifies the darkest color in the image. Colors darker than the black point are set to zero. Mid point specifies a gamma correction to apply to the image. White point specifies the lightest color in the image. Colors brighter than the white point are set to the maximum quantum value.

The format of the MagickLevelImage method is:
  unsigned int MagickLevelImage\( MagickWand *wand, const double black_point, const double gamma,
                               const double white_point );
wand:
  The magick wand.
black_point:
  The black point.
gamma:
  The gamma.
white_point:
  The white point.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (black_point :double)
  (gamma :double)
  (white_point :double))

(defcfun ("MagickLevelImageChannel" %MagickLevelImageChannel)
    %magick-status
  "Adjusts the levels of the specified channel of the reference image by scaling the colors falling between specified white and black points to the full available quantum range. The parameters provided represent the black, mid, and white points. The black point specifies the darkest color in the image. Colors darker than the black point are set to zero. Mid point specifies a gamma correction to apply to the image. White point specifies the lightest color in the image. Colors brighter than the white point are set to the maximum quantum value.

The format of the MagickLevelImageChannel method is:
  unsigned int MagickLevelImageChannel\( MagickWand *wand, const ChannelType channel,
                                      const double black_point, const double gamma,
                                      const double white_point );
wand:
  The magick wand.
channel:
  Identify which channel to level: RedChannel, GreenChannel,
BlueChannel, OpacityChannel, CyanChannel, MagentaChannel, YellowChannel,
BlackChannel.
black_point:
  The black point.
gamma:
  The gamma.
white_point:
  The white point.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (channel %ChannelType)
  (black_point :double)
  (gamma :double)
  (white_point :double))

(defcfun ("MagickMagnifyImage" %MagickMagnifyImage)
    %magick-status
  "Is a convenience method that scales an image proportionally to twice its original size.

The format of the MagickMagnifyImage method is:
  unsigned int MagickMagnifyImage\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickMapImage" %MagickMapImage)
    %magick-status
  "Replaces the colors of an image with the closest color from a reference image.

The format of the MagickMapImage method is:
  unsigned int MagickMapImage\( MagickWand *wand, const MagickWand *map_wand,
                             const unsigned int dither );
wand:
  The magick wand.
map:
  The map wand.
dither:
  Set this integer value to something other than zero to dither
the mapped image.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (map_wand %MagickWand)
  (dither :unsigned-int))

(defcfun ("MagickMatteFloodfillImage" %MagickMatteFloodfillImage)
    %magick-status
  "Changes the transparency value of any pixel that matches target and is an immediate neighbor. If the method FillToBorderMethod is specified, the transparency value is changed for any neighbor pixel that does not match the bordercolor member of image.

The format of the MagickMatteFloodfillImage method is:
  unsigned int MagickMatteFloodfillImage\( MagickWand *wand, const Quantum opacity,
                                        const double fuzz, const PixelWand *bordercolor,
                                        const long x, const long y );
wand:
  The magick wand.
opacity:
  The opacity.
fuzz:
  By default target must match a particular pixel color
exactly.  However, in many cases two colors may differ by a small amount.
The fuzz member of image defines how much tolerance is acceptable to
consider two colors as the same.  For example, set fuzz to 10 and the
color red at intensities of 100 and 102 respectively are now interpreted
as the same color for the purposes of the floodfill.
bordercolor:
  The border color pixel wand.
x,y:
  The starting location of the operation.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (opacity %Quantum)
  (fuzz :double)
  (bordercolor %PixelWand)
  (x :long)
  (y :long))

(defcfun ("MagickMedianFilterImage" %MagickMedianFilterImage)
    %magick-status
  "Applies a digital filter that improves the quality of a noisy image. Each pixel is replaced by the median in a set of neighboring pixels as defined by radius.

The format of the MagickMedianFilterImage method is:
  unsigned int MagickMedianFilterImage\( MagickWand *wand, const double radius );
wand:
  The magick wand.
radius:
  The radius of the pixel neighborhood.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (radius :double))

(defcfun ("MagickMinifyImage" %MagickMinifyImage)
    %magick-status
  "Is a convenience method that scales an image proportionally to one-half its original size

The format of the MagickMinifyImage method is:
  unsigned int MagickMinifyImage\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickModulateImage" %MagickModulateImage)
    %magick-status
  "Lets you control the brightness, saturation, and hue of an image.

The format of the MagickModulateImage method is:
  unsigned int MagickModulateImage\( MagickWand *wand, const double brightness,
                                  const double saturation, const double hue );
wand:
  The magick wand.
brightness:
  The percent change in brighness \(-100 thru +100).
saturation:
  The percent change in saturation \(-100 thru +100)
hue:
  The percent change in hue \(-100 thru +100)

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (brightness :double)
  (saturation :double)
  (hue :double))

(defcfun ("MagickMontageImage" %MagickMontageImage)
    %MagickWand
  "Use MagickMontageImage\() to create a composite image by combining several separate images. The images are tiled on the composite image with the name of the image optionally appearing just below the individual tile.

The format of the MagickMontageImage method is:
  MagickWand MagickMontageImage\( MagickWand *wand, const DrawingWand drawing_wand,
                               const char *tile_geometry, const char *thumbnail_geometry,
                               const MontageMode mode, const char *frame );
wand:
  The magick wand.
drawing_wand:
  The drawing wand.  The font name, size, and color are
obtained from this wand.
tile_geometry:
  the number of tiles per row and page \(e.g. 6x4+0+0).
thumbnail_geometry:
  Preferred image size and border size of each
thumbnail \(e.g. 120x120+4+3>).
mode:
  Thumbnail framing mode: Frame, Unframe, or Concatenate.
frame:
  Surround the image with an ornamental border \(e.g. 15x15+3+3).
The frame color is that of the thumbnail's matte color.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (drawing_wand %DrawingWand)
  (tile_geometry :string)
  (thumbnail_geometry :string)
  (mode %MontageMode)
  (frame :string))

(defcfun ("MagickMorphImages" %MagickMorphImages)
    %MagickWand
  "Method morphs a set of images. Both the image pixels and size are linearly interpolated to give the appearance of a meta-morphosis from one image to the next.

The format of the MagickMorphImages method is:
  MagickWand *MagickMorphImages\( MagickWand *wand, const unsigned long number_frames );
wand:
  The magick wand.
number_frames:
  The number of in-between images to generate.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (number_frames :unsigned-long))

(defcfun ("MagickMosaicImages" %MagickMosaicImages)
    %MagickWand
  "Inlays an image sequence to form a single coherent picture. It returns a wand with each image in the sequence composited at the location defined by the page offset of the image.

The format of the MagickMosaicImages method is:
  MagickWand *MagickMosaicImages\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickMotionBlurImage" %MagickMotionBlurImage)
    %magick-status
  "Simulates motion blur. We convolve the image with a Gaussian operator of the given radius and standard deviation \(sigma). For reasonable results, radius should be larger than sigma. Use a radius of 0 and MotionBlurImage\() selects a suitable radius for you. Angle gives the angle of the blurring motion.

The format of the MagickMotionBlurImage method is:
  unsigned int MagickMotionBlurImage\( MagickWand *wand, const double radius, const double sigma,
                                    const double angle );
wand:
  The magick wand.
radius:
  The radius of the Gaussian, in pixels, not counting
the center pixel.
sigma:
  The standard deviation of the Gaussian, in pixels.
angle:
  Apply the effect along this angle.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (radius :double)
  (sigma :double)
  (angle :double))

(defcfun ("MagickNegateImage" %MagickNegateImage)
    %magick-status
  "Negates the colors in the reference image. The Grayscale option means that only grayscale values within the image are negated.

You can also reduce the influence of a particular channel with a gamma
value of 0.

The format of the MagickNegateImage method is:
  unsigned int MagickNegateImage\( MagickWand *wand, const unsigned int gray );

A description of each parameter follows:
wand:
  The magick wand.
gray:
  If True, only negate grayscale pixels within the image.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (gray :unsigned-int))

(defcfun ("MagickNegateImageChannel" %MagickNegateImageChannel)
    %magick-status
  "Negates the colors in the specified channel of the reference image. The Grayscale option means that only grayscale values within the image are negated. Note that the Grayscale option has no effect for GraphicsMagick.

You can also reduce the influence of a particular channel with a gamma
value of 0.

The format of the MagickNegateImageChannel method is:
  unsigned int MagickNegateImageChannel\( MagickWand *wand, const ChannelType channel,
                                       const unsigned int gray );
wand:
  The magick wand.
channel:
  Identify which channel to extract: RedChannel, GreenChannel,
BlueChannel, OpacityChannel, CyanChannel, MagentaChannel, YellowChannel,
BlackChannel.
gray:
  If True, only negate grayscale pixels within the image.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (channel %ChannelType)
  (gray :unsigned-int))

(defcfun ("MagickNextImage" %MagickNextImage)
    %magick-status
  "Associates the next image in the image list with a magick wand. True is returned if the Wand iterated to a next image, or False is returned if the wand did not iterate to a next image.

The format of the MagickNextImage method is:
  unsigned int MagickNextImage\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickNormalizeImage" %MagickNormalizeImage)
    %magick-status
  "Enhances the contrast of a color image by adjusting the pixels color to span the entire range of colors available

You can also reduce the influence of a particular channel with a gamma
value of 0.

The format of the MagickNormalizeImage method is:
  unsigned int MagickNormalizeImage\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickOilPaintImage" %MagickOilPaintImage)
    %magick-status
  "Applies a special effect filter that simulates an oil painting. Each pixel is replaced by the most frequent color occurring in a circular region defined by radius.

The format of the MagickOilPaintImage method is:
  unsigned int MagickOilPaintImage\( MagickWand *wand, const double radius );

A description of each parameter follows:
wand:
  The magick wand.
radius:
  The radius of the circular neighborhood.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (radius :double))

(defcfun ("MagickOpaqueImage" %MagickOpaqueImage)
    %magick-status
  "Changes any pixel that matches color with the color defined by fill.

The format of the MagickOpaqueImage method is:
  unsigned int MagickOpaqueImage\( MagickWand *wand, const PixelWand *target,
                                const PixelWand *fill, const double fuzz );
wand:
  The magick wand.
target:
  Change this target color to the fill color within the image.
fill:
  The fill pixel wand.
fuzz:
  By default target must match a particular pixel color
exactly.  However, in many cases two colors may differ by a small amount.
The fuzz member of image defines how much tolerance is acceptable to
consider two colors as the same.  For example, set fuzz to 10 and the
color red at intensities of 100 and 102 respectively are now interpreted
as the same color for the purposes of the floodfill.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (target %PixelWand)
  (fill %PixelWand)
  (fuzz :double))

(defcfun ("MagickOperatorImageChannel" %MagickOperatorImageChannel)
    %magick-status
  "Performs the requested arithmetic,
bitwise-logical, or value operation on the selected channels of
the entire image.  The AllChannels channel option operates on all
color channels whereas the GrayChannel channel option treats the
color channels as a grayscale intensity.

These operations are on the DirectClass pixels of the image and do not
  update pixel indexes or colormap.

The format of the MagickOperatorImageChannel method is:
  MagickPassFail MagickOperatorImageChannel\( MagickWand *wand,
      const ChannelType channel,const QuantumOperator quantum_operator,
      const double rvalue )

A description of each parameter follows:
  wand: The magick wand.
  channel: Channel to operate on \(RedChannel, CyanChannel,
      GreenChannel, MagentaChannel, BlueChannel, YellowChannel,
      OpacityChannel, BlackChannel, MatteChannel, AllChannels,
      GrayChannel).  The AllChannels type only updates color
      channels.  The GrayChannel type treats the color channels
      as if they represent an intensity.
  quantum_operator: Operator to use \(AddQuantumOp,AndQuantumOp,
      AssignQuantumOp, DepthQuantumOp, DivideQuantumOp, GammaQuantumOp,
      LShiftQuantumOp, MultiplyQuantumOp,  NegateQuantumOp,
      NoiseGaussianQuantumOp, NoiseImpulseQuantumOp,
      NoiseLaplacianQuantumOp, NoiseMultiplicativeQuantumOp,
      NoisePoissonQuantumOp, NoiseRandomQuantumOp, NoiseUniformQuantumOp,
      OrQuantumOp, RShiftQuantumOp, SubtractQuantumOp,
      ThresholdBlackQuantumOp, ThresholdQuantumOp, ThresholdWhiteQuantumOp,
      ThresholdBlackNegateQuantumOp, ThresholdWhiteNegateQuantumOp,
      XorQuantumOp).
  rvalue: Operator argument.

Since GraphicsMagick v1.3.20"
  (wand %MagickWand)
  (channel %ChannelType)
  (quantum_operator %QuantumOperator) ;; TODO
  (rvalue :double))

(defcfun ("MagickPingImage" %MagickPingImage)
    %magick-status
  "Is like MagickReadImage\() except the only valid information returned is the image width, height, size, and format. It is designed to efficiently obtain this information from a file without reading the entire image sequence into memory.

The format of the MagickPingImage method is:
  unsigned int MagickPingImage\( MagickWand *wand, const char *filename );

A description of each parameter follows:
wand:
  The magick wand.
filename:
  The image filename.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (filename :string))

(defcfun ("MagickPreviewImages" %MagickPreviewImages)
    %MagickWand
  "Tiles 9 thumbnails of the specified image with an image processing operation applied at varying strengths. This is helpful to quickly pin-point an appropriate parameter for an image processing operation.

The format of the MagickPreviewImages method is:
  MagickWand *MagickPreviewImages\( MagickWand *wand, const PreviewType preview );
wand:
  The magick wand.
preview:
  The preview type.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (preview %PreviewType))

(defcfun ("MagickPreviousImage" %MagickPreviousImage)
    %magick-status
  "Selects the previous image associated with a magick wand.

The format of the MagickPreviousImage method is:
  unsigned int MagickPreviousImage\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickProfileImage" %MagickProfileImage)
    %magick-status
  "Use MagickProfileImage\() to add or remove a ICC, IPTC, or generic profile from an image. If the profile is NULL, it is removed from the image otherwise added. Use a name of '*' and a profile of NULL to remove all profiles from the image.

The format of the MagickProfileImage method is:
  unsigned int MagickProfileImage\( MagickWand *wand, const char *name,
                                 const unsigned char *profile, const size_t length );
wand:
  The magick wand.
name:
  Name of profile to add or remove: ICC, IPTC, or generic profile.
profile:
  The profile.
length:
  The length of the profile.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (name :string)
  (profile :pointer)
  (length %size_t))

(defcfun ("MagickQuantizeImage" %MagickQuantizeImage)
    %magick-status
  "Analyzes the colors within a reference image and chooses a fixed number of colors to represent the image. The goal of the algorithm is to minimize the color difference between the input and output image while minimizing the processing time.

The format of the MagickQuantizeImage method is:
  unsigned int MagickQuantizeImage\( MagickWand *wand, const unsigned long number_colors,
                                  const ColorspaceType colorspace,
                                  const unsigned long treedepth, const unsigned int dither,
                                  const unsigned int measure_error );
wand:
  The magick wand.
number_colors:
  The number of colors.
colorspace:
  Perform color reduction in this colorspace, typically
RGBColorspace.
treedepth:
  Normally, this integer value is zero or one.  A zero or
one tells Quantize to choose a optimal tree depth of Log4\(number_colors).%      A tree of this depth generally allows the best representation of the
reference image with the least amount of memory and the fastest
computational speed.  In some cases, such as an image with low color
dispersion \(a few number of colors), a value other than
Log4\(number_colors) is required.  To expand the color tree completely,
use a value of 8.
dither:
  A value other than zero distributes the difference between an
original image and the corresponding color reduced algorithm to
neighboring pixels along a Hilbert curve.
measure_error:
  A value other than zero measures the difference between
the original and quantized images.  This difference is the total
quantization error.  The error is computed by summing over all pixels
in an image the distance squared in RGB space between each reference
pixel value and its quantized value.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (number_colors :unsigned-long)
  (colorspace %ColorspaceType)
  (treedepth :unsigned-long)
  (dither :unsigned-int)
  (measure_error :unsigned-int))

(defcfun ("MagickQuantizeImages" %MagickQuantizeImages)
    %magick-status
  "Analyzes the colors within a sequence of images and chooses a fixed number of colors to represent the image. The goal of the algorithm is to minimize the color difference between the input and output image while minimizing the processing time.

The format of the MagickQuantizeImages method is:
  unsigned int MagickQuantizeImages\( MagickWand *wand, const unsigned long number_colors,
                                   const ColorspaceType colorspace,
                                   const unsigned long treedepth, const unsigned int dither,
                                   const unsigned int measure_error );
wand:
  The magick wand.
number_colors:
  The number of colors.
colorspace:
  Perform color reduction in this colorspace, typically
RGBColorspace.
treedepth:
  Normally, this integer value is zero or one.  A zero or
one tells Quantize to choose a optimal tree depth of Log4\(number_colors).%      A tree of this depth generally allows the best representation of the
reference image with the least amount of memory and the fastest
computational speed.  In some cases, such as an image with low color
dispersion \(a few number of colors), a value other than
Log4\(number_colors) is required.  To expand the color tree completely,
use a value of 8.
dither:
  A value other than zero distributes the difference between an
original image and the corresponding color reduced algorithm to
neighboring pixels along a Hilbert curve.
measure_error:
  A value other than zero measures the difference between
the original and quantized images.  This difference is the total
quantization error.  The error is computed by summing over all pixels
in an image the distance squared in RGB space between each reference
pixel value and its quantized value.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (number_colors :unsigned-long)
  (colorspace %ColorspaceType)
  (treedepth :unsigned-long)
  (dither :unsigned-int)
  (measure_error :unsigned-int))

(defcfun ("MagickQueryFontMetrics" %MagickQueryFontMetrics)
    :pointer
  "Return a pointer to a double array with 7 elements:
0 character width 
1 character height 
2 ascender 
3 descender 
4 text width 
5 text height 
6 maximum horizontal advance

The format of the MagickQueryFontMetrics method is:
  double *MagickQueryFontMetrics\( MagickWand *wand, const DrawingWand *drawing_wand,
                                const char *text );
wand:
  The Magick wand.
drawing_wand:
  The drawing wand.
text:
  The text.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (drawing_wand %DrawingWand)
  (text :string))

(defcfun ("MagickQueryFonts" %MagickQueryFonts)
    :pointer
  "Returns any font that match the specified pattern.

The format of the MagickQueryFonts function is:
  char ** MagickQueryFonts\( const char *pattern, unsigned long *number_fonts );

A description of each parameter follows:
pattern:
  Specifies a pointer to a text string containing a pattern.
number_fonts:
  This integer returns the number of fonts in the list.

Since GraphicsMagick v1.1.0"
  (pattern :string)
  (number_fonts :pointer))

(defcfun ("MagickQueryFormats" %MagickQueryFormats)
    :pointer
  "Returns any image formats that match the specified pattern.

The format of the MagickQueryFormats function is:
  char ** MagickQueryFormats\( const char *pattern, unsigned long *number_formats );
pattern:
  Specifies a pointer to a text string containing a pattern.
number_formats:
  This integer returns the number of image formats in the
list.

Since GraphicsMagick v1.1.0"
  (pattern :string)
  (number_formats :pointer))

(defcfun ("MagickRadialBlurImage" %MagickRadialBlurImage)
    %magick-status
  "Radial blurs an image.

The format of the MagickRadialBlurImage method is:
  unsigned int MagickRadialBlurImage\( MagickWand *wand, const double angle );

A description of each parameter follows:
wand:
  The magick wand.
angle:
  The angle of the blur in degrees.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (angle :double))

(defcfun ("MagickRaiseImage" %MagickRaiseImage)
    %magick-status
  "Creates a simulated three-dimensional button-like effect by lightening and darkening the edges of the image. Members width and height of raise_info define the width of the vertical and horizontal edge of the effect.

The format of the MagickRaiseImage method is:
  unsigned int MagickRaiseImage\( MagickWand *wand, const unsigned long width,
                               const unsigned long height, const long x, const long y,
                               const unsigned int raise_flag );
wand:
  The magick wand.
width,height,x,y:
  Define the dimensions of the area to raise.
raise_flag:
  A value other than zero creates a 3-D raise effect,
otherwise it has a lowered effect.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (width :unsigned-long)
  (height :unsigned-long)
  (x :long)
  (y :long)
  (raise_flag :unsigned-int))

(defcfun ("MagickReadImage" %MagickReadImage)
    %magick-status
  "Reads an image or image sequence.

The format of the MagickReadImage method is:
  unsigned int MagickReadImage\( MagickWand *wand, const char *filename );

A description of each parameter follows:
wand:
  The magick wand.
filename:
  The image filename.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (filename :string))

(defcfun ("MagickReadImageBlob" %MagickReadImageBlob)
    %magick-status
  "Reads an image or image sequence from a blob.

The format of the MagickReadImageBlob method is:
  unsigned int MagickReadImageBlob\( MagickWand *wand, const unsigned char *blob,
                                  const size_t length );
wand:
  The magick wand.
blob:
  The blob.
length:
  The blob length.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (blob :pointer)
  (length %size_t))

(defcfun ("MagickReadImageFile" %MagickReadImageFile)
    %magick-status
  "Reads an image or image sequence from an open file descriptor.

The format of the MagickReadImageFile method is:
  unsigned int MagickReadImageFile\( MagickWand *wand, FILE *file );

A description of each parameter follows:
wand:
  The magick wand.
file:
  The file descriptor.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (file %FILE))

(defcfun ("MagickReduceNoiseImage" %MagickReduceNoiseImage)
    %magick-status
  "Smooths the contours of an image while still preserving edge information. The algorithm works by replacing each pixel with its neighbor closest in value. A neighbor is defined by radius. Use a radius of 0 and ReduceNoise\() selects a suitable radius for you.

The format of the MagickReduceNoiseImage method is:
  unsigned int MagickReduceNoiseImage\( MagickWand *wand, const double radius );

A description of each parameter follows:
wand:
  The magick wand.
radius:
  The radius of the pixel neighborhood.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (radius :double))

(defcfun ("MagickRelinquishMemory" %MagickRelinquishMemory)
    %magick-status
  "Relinquishes memory resources returned by such methods as MagickDescribeImage\(), MagickGetException\(), etc.

The format of the MagickRelinquishMemory method is:
  unsigned int MagickRelinquishMemory\( void *resource );

A description of each parameter follows:
resource:
  Relinquish the memory associated with this resource.

Since GraphicsMagick v1.1.0"
  (resource :pointer))

(defcfun ("MagickRemoveImage" %MagickRemoveImage)
    %magick-status
  "Removes an image from the image list.

The format of the MagickRemoveImage method is:
  unsigned int MagickRemoveImage\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickRemoveImageOption" %MagickRemoveImageOption)
    %magick-status
  "Removes an image format-specific option from the the image \(.e.g MagickRemoveImageOption(wand,\"jpeg\",\"preserve-settings\").

The format of the MagickRemoveImageOption method is:
  unsigned int MagickRemoveImageOption\( MagickWand *wand, const char *format,
                                      const char *key );
wand:
  The magick wand.
format:
  The image format.
key:
  The key.

Since GraphicsMagick v1.3.26"
  (wand %MagickWand)
  (format :string)
  (key :string))

(defcfun ("MagickRemoveImageProfile" %MagickRemoveImageProfile)
    :pointer
  "Removes the named image profile and returns it.

The format of the MagickRemoveImageProfile method is:
  unsigned char *MagickRemoveImageProfile\( MagickWand *wand, const char *name,
                                         unsigned long *length );
wand:
  The magick wand.
name:
  Name of profile to return: ICC, IPTC, or generic profile.
length:
  The length of the profile.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (name :string)
  (length :pointer))

(defcfun ("MagickResetIterator" %MagickResetIterator)
    :void
  "Resets the wand iterator. Use it in conjunction with MagickNextImage\() to iterate over all the images in a wand container.

The format of the MagickReset method is:
  void MagickResetIterator\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickResampleImage" %MagickResampleImage)
    %magick-status
  "Resample image to desired resolution.

Bessel   Blackman   Box
Catrom   Cubic      Gaussian
Hanning  Hermite    Lanczos
Mitchell Point      Quandratic
Sinc     Triangle

Most of the filters are FIR \(finite impulse response), however, Bessel,
Gaussian, and Sinc are IIR \(infinite impulse response).  Bessel and Sinc
are windowed \(brought down to zero) with the Blackman filter.

The format of the MagickResampleImage method is:
  unsigned int MagickResampleImage\( MagickWand *wand, const double x_resolution,
                                  const double y_resolution, const FilterTypes filter,
                                  const double blur );
wand:
  The magick wand.
x_resolution:
  The new image x resolution.
y_resolution:
  The new image y resolution.
filter:
  Image filter to use.
blur:
  The blur factor where > 1 is blurry, < 1 is sharp.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (x_resolution :double)
  (y_resolution :double)
  (filter %FilterTypes)
  (blur :double))

(defcfun ("MagickResizeImage" %MagickResizeImage)
    %magick-status
  "Scales an image to the desired dimensions with one of these filters:

Bessel   Blackman   Box
Catrom   Cubic      Gaussian
Hanning  Hermite    Lanczos
Mitchell Point      Quandratic
Sinc     Triangle

Most of the filters are FIR \(finite impulse response), however, Bessel,
Gaussian, and Sinc are IIR \(infinite impulse response).  Bessel and Sinc
are windowed \(brought down to zero) with the Blackman filter.

The format of the MagickResizeImage method is:
  unsigned int MagickResizeImage\( MagickWand *wand, const unsigned long columns,
                                const unsigned long rows, const FilterTypes filter,
                                const double blur );
wand:
  The magick wand.
columns:
  The number of columns in the scaled image.
rows:
  The number of rows in the scaled image.
filter:
  Image filter to use.
blur:
  The blur factor where > 1 is blurry, < 1 is sharp.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (columns :unsigned-long)
  (rows :unsigned-long)
  (filter %FilterTypes)
  (blur %double))

(defcfun ("MagickRollImage" %MagickRollImage)
    %magick-status
  "Offsets an image as defined by x_offset and y_offset.

The format of the MagickRollImage method is:
  unsigned int MagickRollImage\( MagickWand *wand, const long x_offset,
                              const long y_offset );
wand:
  The magick wand.
x_offset:
  The x offset.
y_offset:
  The y offset.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (x_offset :long)
  (y_offset :long))

(defcfun ("MagickRotateImage" %MagickRotateImage)
    %magick-status
  "Rotates an image the specified number of degrees. Empty triangles left over from rotating the image are filled with the background color.

The format of the MagickRotateImage method is:
  unsigned int MagickRotateImage\( MagickWand *wand, const PixelWand *background,
                                const double degrees );
wand:
  The magick wand.
background:
  The background pixel wand.
degrees:
  The number of degrees to rotate the image.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (background %PixelWand)
  (degrees %double))

(defcfun ("MagickSampleImage" %MagickSampleImage)
    %magick-status
  "Scales an image to the desired dimensions with pixel sampling. Unlike other scaling methods, this method does not introduce any additional color into the scaled image.

The format of the MagickSampleImage method is:
  unsigned int MagickSampleImage\( MagickWand *wand, const unsigned long columns,
                                const unsigned long rows );
wand:
  The magick wand.
columns:
  The number of columns in the scaled image.
rows:
  The number of rows in the scaled image.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (columns :unsigned-long)
  (rows :unsigned-long))

(defcfun ("MagickScaleImage" %MagickScaleImage)
    %magick-status
  "Scales the size of an image to the given dimensions.

The format of the MagickScaleImage method is:
  unsigned int MagickScaleImage\( MagickWand *wand, const unsigned long columns,
                               const unsigned long rows );
wand:
  The magick wand.
columns:
  The number of columns in the scaled image.
rows:
  The number of rows in the scaled image.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (columns :unsigned-long)
  (rows :unsigned-long))

(defcfun ("MagickSeparateImageChannel" %MagickSeparateImageChannel)
    %magick-status
  "Separates a channel from the image and returns a grayscale image. A channel is a particular color component of each pixel in the image.

The format of the MagickChannelImage method is:
  unsigned int MagickSeparateImageChannel\( MagickWand *wand, const ChannelType channel );
wand:
  The magick wand.
channel:
  Identify which channel to extract: RedChannel, GreenChannel,
BlueChannel, OpacityChannel, CyanChannel, MagentaChannel, YellowChannel,
BlackChannel.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (channel %ChannelType))

(defcfun ("MagickSetCompressionQuality" %MagickSetCompressionQuality)
    %magick-status
  "Sets the image quality factor, which determines compression options when saving the file.

For the JPEG and MPEG image formats, quality is 0 \(lowest image
quality and highest compression) to 100 \(best quality but least
effective compression).  The default quality is 75.  Use the
-sampling-factor option to specify the factors for chroma
downsampling.  To use the same quality value as that found by the
JPEG decoder, use the -define jpeg:preserve-settings flag.

For the MIFF image format, and the TIFF format while using ZIP
compression, quality/10 is the zlib compres- sion level, which is 0
\(worst but fastest compression) to 9 \(best but slowest). It has no
effect on the image appearance, since the compression is always
lossless.

For the JPEG-2000 image format, quality is mapped using a non-linear
equation to the compression ratio required by the Jasper library.
This non-linear equation is intended to loosely approximate the
quality provided by the JPEG v1 format.  The default quality value 75
results in a request for 16:1 compression. The quality value 100
results in a request for non-lossy compres- sion.

For the MNG and PNG image formats, the quality value sets the zlib
compression level \(quality / 10) and filter-type \(quality % 10).
Compression levels range from 0 \(fastest compression) to 100 \(best
but slowest).  For compression level 0, the Huffman-only strategy is
used, which is fastest but not necessarily the worst compression.  If
filter-type is 4 or less, the specified filter-type is used for all
scanlines:
none
sub
up
average
Paeth

If filter-type is 5, adaptive filtering is used when quality is
greater than 50 and the image does not have a color map, otherwise no
filtering is used.

If filter-type is 6, adaptive filtering with minimum-
sum-of-absolute-values is used.

Only if the output is MNG, if filter-type is 7, the LOCO color
transformation and adaptive filtering with
minimum-sum-of-absolute-values are used.

The default is quality is 75, which means nearly the best compression
with adaptive filtering.  The quality setting has no effect on the
appearance of PNG and MNG images, since the compression is always
lossless.

For further information, see the PNG specification.

When writing a JNG image with transparency, two quality values are
required, one for the main image and one for the grayscale image that
conveys the opacity channel.  These are written as a single integer
equal to the main image quality plus 1000 times the opacity quality.
For example, if you want to use quality 75 for the main image and
quality 90 to compress the opacity data, use -quality 90075.

For the PNM family of formats \(PNM, PGM, and PPM) specify a quality
factor of zero in order to obtain the ASCII variant of the
format. Note that -compress none used to be used to trigger ASCII
output but provided the opposite result of what was expected as
compared with other formats.

The format of the MagickSetCompressionQuality method is:
  unsigned int MagickSetCompressionQuality\( MagickWand *wand, const unsigned long quality );
wand:
  The magick wand.
delay:
  The image quality.

Since GraphicsMagick v1.3.7"
  (wand %MagickWand)
  (quality :unsigned-long))

(defcfun ("MagickSetDepth" %MagickSetDepth)
    %magick-status
  "Sets the sample depth to be used when reading from a raw image or a format which requires that the depth be specified in advance by the user.

The format of the MagickSetDepth method is:
  unsigned int MagickSetDepth\( MagickWand *wand, const size_t depth );

A description of each parameter follows:
wand:
  The magick wand.
depth:
  The sample depth.

Since GraphicsMagick v1.3.13"
  (wand %MagickWand)
  (depth %size_t))

(defcfun ("MagickSetFilename" %MagickSetFilename)
    %magick-status
  "Sets the filename before you read or write an image file.

The format of the MagickSetFilename method is:
  unsigned int MagickSetFilename\( MagickWand *wand, const char *filename );

A description of each parameter follows:
wand:
  The magick wand.
filename:
  The image filename.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (filename :string))

(defcfun ("MagickSetFormat" %MagickSetFormat)
    %magick-status
  "Sets the file or blob format \(e.g. \"BMP\") to be used when a file or blob is read. Usually this is not necessary because GraphicsMagick is able to auto-detect the format based on the file header \(or the file extension), but some formats do not use a unique header or the selection may be ambigious. Use MagickSetImageFormat\() to set the format to be used when a file or blob is to be written.

The format of the MagickSetFormat method is:
  unsigned int MagickSetFormat\( MagickWand *wand, const char *format );

A description of each parameter follows:
wand:
  The magick wand.
filename:
  The file or blob format.

Since GraphicsMagick v1.3.13"
  (wand %MagickWand)
  (format :string))

(defcfun ("MagickSetImage" %MagickSetImage)
    %magick-status
  "Replaces the last image returned by MagickSetImageIndex\(), MagickNextImage\(), MagickPreviousImage\() with the images from the specified wand.

The format of the MagickSetImage method is:
  unsigned int MagickSetImage\( MagickWand *wand, const MagickWand *set_wand );

A description of each parameter follows:
wand:
  The magick wand.
set_wand:
  The set_wand wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (set_wand %MagickWand))

(defcfun ("MagickSetImageAttribute" %MagickSetImageAttribute)
    %magick-status
  "MagickSetImageAttribute sets an image attribute

The format of the MagickSetImageAttribute method is:
  unsigned int MagickSetImageAttribute\( MagickWand *wand, const char *name,
                                      const char *value );

A description of each parameter follows:
wand:
  The magick wand.
name:
  The name of the attribute
value:
  The value of the attribute

Since GraphicsMagick v1.3.8"
  (wand %MagickWand)
  (name :string)
  (value :string))

(defcfun ("MagickSetImageBackgroundColor" %MagickSetImageBackgroundColor)
    %magick-status
  "Sets the image background color.

The format of the MagickSetImageBackgroundColor method is:
  unsigned int MagickSetImageBackgroundColor\( MagickWand *wand, const PixelWand *background );
wand:
  The magick wand.
background:
  The background pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (background %PixelWand))

(defcfun ("MagickSetImageBluePrimary" %MagickSetImageBluePrimary)
    %magick-status
  "Sets the image chromaticity blue primary point.

The format of the MagickSetImageBluePrimary method is:
  unsigned int MagickSetImageBluePrimary\( MagickWand *wand, const double x, const double y );
wand:
  The magick wand.
x:
  The blue primary x-point.
y:
  The blue primary y-point.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (x :double)
  (y :double))

(defcfun ("MagickSetImageBorderColor" %MagickSetImageBorderColor)
    %magick-status
  "Sets the image border color.

The format of the MagickSetImageBorderColor method is:
  unsigned int MagickSetImageBorderColor\( MagickWand *wand, const PixelWand *border );
wand:
  The magick wand.
border:
  The border pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (border %PixelWand))

(defcfun ("MagickSetImageColormapColor" %MagickSetImageColormapColor)
    %magick-status
  "Sets the color of the specified colormap index.

The format of the MagickSetImageColormapColor method is:
  unsigned int MagickSetImageColormapColor\( MagickWand *wand, const unsigned long index,
                                          const PixelWand *color );
wand:
  The magick wand.
index:
  The offset into the image colormap.
color:
  Return the colormap color in this wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (index :unsigned-long)
  (color %PixelWand))

(defcfun ("MagickSetImageColorspace" %MagickSetImageColorspace)
    %magick-status
  "Sets the image colorspace.

The format of the MagickSetImageColorspace method is:
  unsigned int MagickSetImageColorspace\( MagickWand *wand, const ColorspaceType colorspace );
wand:
  The magick wand.
colorspace:
  The image colorspace:   UndefinedColorspace, RGBColorspace,
GRAYColorspace, TransparentColorspace, OHTAColorspace, XYZColorspace,
YCbCrColorspace, YCCColorspace, YIQColorspace, YPbPrColorspace,
YPbPrColorspace, YUVColorspace, CMYKColorspace, sRGBColorspace,
HSLColorspace, or HWBColorspace.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (colorspace %ColorspaceType))

(defcfun ("MagickSetImageCompose" %MagickSetImageCompose)
    %magick-status
  "Sets the image composite operator, useful for specifying how to composite the image thumbnail when using the MagickMontageImage\() method.

The format of the MagickSetImageCompose method is:
  unsigned int MagickSetImageCompose\( MagickWand *wand, const CompositeOperator compose );
wand:
  The magick wand.
compose:
  The image composite operator.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (compose %CompositeOperator))

(defcfun ("MagickSetImageCompression" %MagickSetImageCompression)
    %magick-status
  "Sets the image compression.

The format of the MagickSetImageCompression method is:
  unsigned int MagickSetImageCompression\( MagickWand *wand,
                                        const CompressionType compression );
wand:
  The magick wand.
compression:
  The image compression type.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (compression %CompressionType))

(defcfun ("MagickSetImageDelay" %MagickSetImageDelay)
    %magick-status
  "Sets the image delay.

The format of the MagickSetImageDelay method is:
  unsigned int MagickSetImageDelay\( MagickWand *wand, const unsigned long delay );
wand:
  The magick wand.
delay:
  The image delay in 1/100th of a second.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (delay :unsigned-long))

(defcfun ("MagickSetImageChannelDepth" %MagickSetImageChannelDepth)
    %magick-status
  "Sets the depth of a particular image channel.

The format of the MagickSetImageChannelDepth method is:
  unsigned int MagickSetImageChannelDepth\( MagickWand *wand, const ChannelType channel,
                                         const unsigned long depth );
wand:
  The magick wand.
channel:
  Identify which channel to extract: RedChannel, GreenChannel,
BlueChannel, OpacityChannel, CyanChannel, MagentaChannel, YellowChannel,
BlackChannel.
depth:
  The image depth in bits.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (channel %ChannelType)
  (depth :unsigned-long))

(defcfun ("MagickSetImageDepth" %MagickSetImageDepth)
    %magick-status
  "Sets the image depth.

The format of the MagickSetImageDepth method is:
  unsigned int MagickSetImageDepth\( MagickWand *wand, const unsigned long depth );
wand:
  The magick wand.
depth:
  The image depth in bits: 8, 16, or 32.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (depth :unsigned-long))

(defcfun ("MagickSetImageDispose" %MagickSetImageDispose)
    %magick-status
  "Sets the image disposal method.

The format of the MagickSetImageDispose method is:
  unsigned int MagickSetImageDispose\( MagickWand *wand, const DisposeType dispose );
wand:
  The magick wand.
dispose:
  The image disposeal type.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (dispose %DisposeType))

(defcfun ("MagickSetImageFilename" %MagickSetImageFilename)
    %magick-status
  "Sets the filename of a particular image in a sequence.

The format of the MagickSetImageFilename method is:
  unsigned int MagickSetImageFilename\( MagickWand *wand, const char *filename );
wand:
  The magick wand.
filename:
  The image filename.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (filename :string))

(defcfun ("MagickSetImageFormat" %MagickSetImageFormat)
    %magick-status
  "Sets the format of a particular image in a sequence. The format is designated by a magick string \(e.g. \"GIF\").

The format of the MagickSetImageFormat method is:
  unsigned int MagickSetImageFormat\( MagickWand *wand, const char *format );
wand:
  The magick wand.
magick:
  The image format.

Since GraphicsMagick v1.3.1"
  (wand %MagickWand)
  (format :string))

(defcfun ("MagickSetImageFuzz" %MagickSetImageFuzz)
    %magick-status
  "Sets the color comparison fuzz factor. Colors closer than the fuzz factor are considered to be the same when comparing colors. Note that some other functions such as MagickColorFloodfillImage\() implicitly set this value.

The format of the MagickSetImageFuzz method is:
  unsigned int MagickSetImageFuzz\( MagickWand *wand, const double fuzz );

A description of each parameter follows:
wand:
  The magick wand.
fuzz:
  The color comparison fuzz factor

Since GraphicsMagick v1.3.8"
  (wand %MagickWand)
  (fuzz :double))

(defcfun ("MagickSetImageGamma" %MagickSetImageGamma)
    %magick-status
  "Sets the image gamma.

The format of the MagickSetImageGamma method is:
  unsigned int MagickSetImageGamma\( MagickWand *wand, const double gamma );

A description of each parameter follows:
wand:
  The magick wand.
gamma:
  The image gamma.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (gamma :double))

(defcfun ("MagickSetImageGeometry" %MagickSetImageGeometry)
    %magick-status
  "Sets the image geometry string.

The format of the MagickSetImageGeometry method is:
  unsigned int MagickSetImageGeometry\( MagickWand *wand, const char *geometry )

A description of each parameter follows:
  wand: The magick wand.
  geometry: The image geometry.

Since GraphicsMagick v1.3.20"
  (wand %MagickWand)
  (geometry :string))

(defcfun ("MagickSetImageGravity" %MagickSetImageGravity)
    %magick-status
  "Sets the image gravity. This is used when evaluating regions defined by a geometry and the image dimensions. It may be used in conjunction with operations which use a geometry parameter to adjust the x, y parameters of the final operation. Gravity is used in composition to determine where the image should be placed within the defined geometry region. It may be used with montage to effect placement of the image within the tile.

The format of the MagickSetImageGravity method is:
  unsigned int MagickSetImageGravity\( MagickWand *wand, const GravityType );

A description of each parameter follows:
wand:
  The magick wand.
gravity:
  The image gravity.  Available values are ForgetGravity,
NorthWestGravity, NorthGravity, NorthEastGravity, WestGravity,
CenterGravity, EastGravity, SouthWestGravity, SouthGravity,
SouthEastGravity, and StaticGravity

Since GraphicsMagick v1.3.20"
  (wand %MagickWand)
  (gravitytype %GravityType))

(defcfun ("MagickSetImageGreenPrimary" %MagickSetImageGreenPrimary)
    %magick-status
  "Sets the image chromaticity green primary point.

The format of the MagickSetImageGreenPrimary method is:
  unsigned int MagickSetImageGreenPrimary\( MagickWand *wand, const double x, const double y );
wand:
  The magick wand.
x:
  The green primary x-point.
y:
  The green primary y-point.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (x %double)
  (y %double))

(defcfun ("MagickSetImageIndex" %MagickSetImageIndex)
    %magick-status
  "Set the current image to the position of the list specified with the index parameter.

The format of the MagickSetImageIndex method is:
  unsigned int MagickSetImageIndex\( MagickWand *wand, const long index );

A description of each parameter follows:
wand:
  The magick wand.
index:
  The scene number.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (index :long))

(defcfun ("MagickSetImageInterlaceScheme" %MagickSetImageInterlaceScheme)
    %magick-status
  "Sets the image interlace scheme. Please use SetInterlaceScheme\() instead to change the interlace scheme used when writing the image.

The format of the MagickSetImageInterlaceScheme method is:
  unsigned int MagickSetImageInterlaceScheme\( MagickWand *wand,
                                            const InterlaceType interlace_scheme );
wand:
  The magick wand.
interlace_scheme:
  The image interlace scheme: NoInterlace, LineInterlace,
PlaneInterlace, PartitionInterlace.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (interlace_scheme %InterlaceType))

(defcfun ("MagickSetImageIterations" %MagickSetImageIterations)
    %magick-status
  "Sets the image iterations.

The format of the MagickSetImageIterations method is:
  unsigned int MagickSetImageIterations\( MagickWand *wand, const unsigned long iterations );
wand:
  The magick wand.
delay:
  The image delay in 1/100th of a second.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (iterations :unsigned-long))

(defcfun ("MagickSetImageMatte" %MagickSetImageMatte)
    %magick-status
  "Sets the image matte flag.  The image opacity \(inverse of transparency) channel is enabled if the matte flag is True.

The format of the MagickSetImageMatte method is:
  unsigned int MagickSetImageMatte\( MagickWand *wand, const unsigned int matte )

A description of each parameter follows:
wand:
  The magick wand.
matte:
  The image matte.

Since GraphicsMagick v1.3.20"
  (wand %MagickWand)
  (matte :unsigned-int))

(defcfun ("MagickSetImageMatteColor" %MagickSetImageMatteColor)
    %magick-status
  "Sets the image matte color.

The format of the MagickSetImageMatteColor method is:
  unsigned int MagickSetImageMatteColor\( MagickWand *wand, const PixelWand *matte );
wand:
  The magick wand.
matte:
  The matte pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (matte %PixelWand))

(defcfun ("MagickSetImageOption" %MagickSetImageOption)
    %magick-status
  "Associates one or options with a particular image format \(.e.g MagickSetImageOption\(wand,\"jpeg\",\"preserve-settings\",\"true\").

The format of the MagickSetImageOption method is:
  unsigned int MagickSetImageOption\( MagickWand *wand, const char *format, const char *key,
                                   const char *value );
wand:
  The magick wand.
format:
  The image format.
key:
  The key.
value:
  The value.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (format :string)
  (key :string)
  (value :string))

(defcfun ("MagickSetImageOrientation" %MagickSetImageOrientation)
    %magick-status
  "Sets the internal image orientation type.
The EXIF orientation tag will be updated if present.

The format of the MagickSetImageOrientation method is:
  MagickSetImageOrientation\( MagickWand *wand, OrientationType new_orientation )

A description of each parameter follows:
wand:
  The magick wand.
new_orientation:
  The new orientation of the image. One of:
      UndefinedOrientation    Image orientation not specified.
      TopLeftOrientation      Left to right and Top to bottom.
      TopRightOrientation     Right to left  and Top to bottom.
      BottomRightOrientation  Right to left and Bottom to top.
      BottomLeftOrientation   Left to right and Bottom to top.
      LeftTopOrientation      Top to bottom and Left to right.
      RightTopOrientation     Top to bottom and Right to left.
      RightBottomOrientation  Bottom to top and Right to left.
      LeftBottomOrientation   Bottom to top and Left to right.

Returns True on success, False otherwise.

Since GraphicsMagick v1.3.26"
  (wand %MagickWand)
  (new_orientation %OrientationType))

(defcfun ("MagickSetImagePage" %MagickSetImagePage)
    %magick-status
  "Sets the image page size and offset used when placing \(e.g. compositing) the image. Pass all zeros for the default placement.

The format of the MagickSetImagePage method is:
  unsigned int MagickSetImagePage\( MagickWand *wand, const unsigned long width,
                                 const unsigned long height, const long x, const long y );
wand:
  The magick wand.
width, height:
  The region size.
x, y:
  Offset \(from top left) on base canvas image on
which to composite image data.

Since GraphicsMagick v1.3.18"
  (wand %MagickWand)
  (width :unsigned-long)
  (height :unsigned-long)
  (x :long)
  (y :long))

(defcfun ("MagickSetImagePixels" %MagickSetImagePixels)
    %magick-status
  "Accepts pixel data and stores it in the image at the location you specify. The method returns False on success otherwise True if an error is encountered. The pixel data can be either char, short int, int, long, float, or double in the order specified by map.

Suppose your want want to upload the first scanline of a 640x480 image from
character data in red-green-blue order:

MagickSetImagePixels\(wand,0,0,0,640,1,\"RGB\",CharPixel,pixels);

The format of the MagickSetImagePixels method is:
  unsigned int MagickSetImagePixels\( MagickWand *wand, const long x_offset, const long y_offset,
                                   const unsigned long columns, const unsigned long rows,
                                   const char *map, const StorageType storage,
                                   unsigned char *pixels );
wand:
  The magick wand.
x_offset, y_offset:
  Offset \(from top left) on base canvas image on
which to composite image data.
columns, rows:
  Dimensions of image.
map:
  This string reflects the expected ordering of the pixel array.
It can be any combination or order of R = red, G = green, B = blue,
A = alpha \(same as Transparency), O = Opacity, T = Transparency,
C = cyan, Y = yellow, M = magenta, K = black, or I = intensity
\(for grayscale). Specify \"P\" = pad, to skip over a quantum which is
intentionally ignored. Creation of an alpha channel for CMYK images
is currently not supported.
storage:
  Define the data type of the pixels.  Float and double types are
expected to be normalized [0..1] otherwise [0..MaxRGB].  Choose from
these types: CharPixel, ShortPixel, IntegerPixel, LongPixel, FloatPixel,
or DoublePixel.
pixels:
  This array of values contain the pixel components as defined by
map and type.  You must preallocate this array where the expected
length varies depending on the values of width, height, map, and type.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (x_offset :long)
  (y_offset :long)
  (columns :unsigned-long)
  (rows :unsigned-long)
  (map :string)
  (storage %StorageType)
  (pixels :pointer))

(defcfun ("MagickSetImageProfile" %MagickSetImageProfile)
    %magick-status
  "Adds a named profile to the magick wand. If a profile with the same name already exists, it is replaced. This method differs from the MagickProfileImage\() method in that it does not apply any CMS color profiles.

The format of the MagickSetImageProfile method is:
  unsigned int MagickSetImageProfile\( MagickWand *wand, const char *name,
                                    const unsigned char *profile,
                                    const unsigned long length );
wand:
  The magick wand.
name:
  Name of profile to add or remove: ICC, IPTC, or generic profile.
profile:
  The profile.
length:
  The length of the profile.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (name :string)
  (profile :pointer)
  (length :unsigned-long))

(defcfun ("MagickSetImageRedPrimary" %MagickSetImageRedPrimary)
    %magick-status
  "Sets the image chromaticity red primary point.

The format of the MagickSetImageRedPrimary method is:
  unsigned int MagickSetImageRedPrimary\( MagickWand *wand, const double x, const double y );
wand:
  The magick wand.
x:
  The red primary x-point.
y:
  The red primary y-point.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (x %double)
  (y %double))

(defcfun ("MagickSetImageRenderingIntent" %MagickSetImageRenderingIntent)
    %magick-status
  "Sets the image rendering intent.

The format of the MagickSetImageRenderingIntent method is:
  unsigned int MagickSetImageRenderingIntent\( MagickWand *wand,
                                            const RenderingIntent rendering_intent );
wand:
  The magick wand.
rendering_intent:
  The image rendering intent: UndefinedIntent,
SaturationIntent, PerceptualIntent, AbsoluteIntent, or RelativeIntent.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (rendering_intent %RenderingIntent))

(defcfun ("MagickSetImageResolution" %MagickSetImageResolution)
    %magick-status
  "Sets the image resolution.

The format of the MagickSetImageResolution method is:
  unsigned int MagickSetImageResolution\( MagickWand *wand, const double x_resolution,
                                       const doubtl y_resolution );
wand:
  The magick wand.
x_resolution:
  The image x resolution.
y_resolution:
  The image y resolution.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (x_resolution %double)
  (y_resolution %double))

(defcfun ("MagickSetImageScene" %MagickSetImageScene)
    %magick-status
  "Sets the image scene.

The format of the MagickSetImageScene method is:
  unsigned int MagickSetImageScene\( MagickWand *wand, const unsigned long scene );
wand:
  The magick wand.
delay:
  The image scene number.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (scene :unsigned-long))

(defcfun ("MagickSetImageType" %MagickSetImageType)
    %magick-status
  "Sets the image type.

The format of the MagickSetImageType method is:
  unsigned int MagickSetImageType\( MagickWand *wand, const ImageType image_type );
wand:
  The magick wand.
image_type:
  The image type:   UndefinedType, BilevelType, GrayscaleType,
GrayscaleMatteType, PaletteType, PaletteMatteType, TrueColorType,
TrueColorMatteType, ColorSeparationType, ColorSeparationMatteType,
or OptimizeType.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (image_type %ImageType))

(defcfun ("MagickSetImageSavedType" %MagickSetImageSavedType)
    %magick-status
  "Sets the image type that will be used when the image is saved.

The format of the MagickSetImageSavedType method is:
  unsigned int MagickSetImageSavedType\( MagickWand *wand, const ImageType image_type );
wand:
  The magick wand.
image_type:
  The image type:   UndefinedType, BilevelType, GrayscaleType,
GrayscaleMatteType, PaletteType, PaletteMatteType, TrueColorType,
TrueColorMatteType, ColorSeparationType, ColorSeparationMatteType,
or OptimizeType.

Since GraphicsMagick v1.3.13"
  (wand %MagickWand)
  (image_type %ImageType))

(defcfun ("MagickSetImageUnits" %MagickSetImageUnits)
    %magick-status
  "Sets the image units of resolution.

The format of the MagickSetImageUnits method is:
  unsigned int MagickSetImageUnits\( MagickWand *wand, const ResolutionType units );
wand:
  The magick wand.
units:
  The image units of resolution : Undefinedresolution,
PixelsPerInchResolution, or PixelsPerCentimeterResolution.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (units %ResolutionType))

(defcfun ("MagickSetImageVirtualPixelMethod" %MagickSetImageVirtualPixelMethod)
    %magick-status
  "Sets the image virtual pixel method.

The format of the MagickSetImageVirtualPixelMethod method is:
  unsigned int MagickSetImageVirtualPixelMethod\( MagickWand *wand,
                                               const VirtualPixelMethod method );
wand:
  The magick wand.
method:
  The image virtual pixel method : UndefinedVirtualPixelMethod,
ConstantVirtualPixelMethod,  EdgeVirtualPixelMethod,
MirrorVirtualPixelMethod, or TileVirtualPixelMethod.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (method %VirtualPixelMethod))

(defcfun ("MagickSetInterlaceScheme" %MagickSetInterlaceScheme)
    %magick-status
  "Sets the interlace scheme used when writing the image.

The format of the MagickSetInterlaceScheme method is:
  unsigned int MagickSetInterlaceScheme\( MagickWand *wand,
                                       const InterlaceType interlace_scheme );
wand:
  The magick wand.
interlace_scheme:
  The image interlace scheme: NoInterlace, LineInterlace,
PlaneInterlace, PartitionInterlace.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (interlace_scheme %InterlaceType))

(defcfun ("MagickSetResolution" %MagickSetResolution)
    %magick-status
  "Sets the resolution \(density) of the magick wand. Set it before you read an EPS, PDF, or Postscript file in order to influence the size of the returned image, or after an image has already been created to influence the rendered image size when used with typesetting software.

Also see MagickSetResolutionUnits\() which specifies the units to use for
the image resolution.

The format of the MagickSetResolution method is:
  unsigned int MagickSetResolution\( MagickWand *wand, const double x_resolution,
                                  const double y_resolution );
wand:
  The magick wand.
x_resolution:
  The horizontal resolution
y_resolution:
  The vertical reesolution

Since GraphicsMagick v1.3.8"
  (wand %MagickWand)
  (x_resolution %double)
  (y_resolution %double))

(defcfun ("MagickSetResolutionUnits" %MagickSetResolutionUnits)
    %magick-status
  "Sets the resolution units of the magick wand. It should be used in conjunction with MagickSetResolution\(). This method works both before and after an image has been read.

Also see MagickSetImageUnits\() which specifies the units which apply to
the image resolution setting after an image has been read.

The format of the MagickSetResolutionUnits method is:
  unsigned int MagickSetResolutionUnits\( MagickWand *wand, const ResolutionType units );
wand:
  The magick wand.
units:
  The image units of resolution : Undefinedresolution,
PixelsPerInchResolution, or PixelsPerCentimeterResolution.

Since GraphicsMagick v1.3.8"
  (wand %MagickWand)
  (units %ResolutionType))

(defcfun ("MagickSetResourceLimit" %MagickSetResourceLimit)
    %magick-status
  "Sets the limit for a particular resource in megabytes.

The format of the MagickSetResourceLimit method is:
  unsigned int MagickSetResourceLimit\( const ResourceType type, const unsigned long *limit );
type:
  The type of resource: DiskResource, FileResource, MapResource,
MemoryResource, PixelsResource, ThreadsResource, WidthResource,
HeightResource.

o The maximum limit for the resource.

Since GraphicsMagick v1.1.0"
  (type %ResourceType)
  (limit :pointer))

(defcfun ("MagickSetSamplingFactors" %MagickSetSamplingFactors)
    %magick-status
  "Sets the image sampling factors.

The format of the MagickSetSamplingFactors method is:
  unsigned int MagickSetSamplingFactors\( MagickWand *wand, const unsigned long number_factors,
                                       const double *sampling_factors );
wand:
  The magick wand.
number_factoes:
  The number of factors.
sampling_factors:
  An array of doubles representing the sampling factor
for each color component \(in RGB order).

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (number_factors :unsigned-long)
  (sampling_factors :pointer))

(defcfun ("MagickSetSize" %MagickSetSize)
    %magick-status
  "Sets the size of the magick wand. Set it before you read a raw image format such as RGB, GRAY, or CMYK.

The format of the MagickSetSize method is:
  unsigned int MagickSetSize\( MagickWand *wand, const unsigned long columns,
                            const unsigned long rows );
wand:
  The magick wand.
columns:
  The width in pixels.
height:
  The height in pixels.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (columns :unsigned-long)
  (rows :unsigned-long))

(defcfun ("MagickSetImageWhitePoint" %MagickSetImageWhitePoint)
    %magick-status
  "Sets the image chromaticity white point.

The format of the MagickSetImageWhitePoint method is:
  unsigned int MagickSetImageWhitePoint\( MagickWand *wand, const double x, const double y );
wand:
  The magick wand.
x:
  The white x-point.
y:
  The white y-point.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (x %double)
  (y %double))

(defcfun ("MagickSetPassphrase" %MagickSetPassphrase)
    %magick-status
  "Sets the passphrase.

The format of the MagickSetPassphrase method is:
  unsigned int MagickSetPassphrase\( MagickWand *wand, const char *passphrase );

A description of each parameter follows:
wand:
  The magick wand.
passphrase:
  The passphrase.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (passphrase :string))

(defcfun ("MagickSharpenImage" %MagickSharpenImage)
    %magick-status
  "Sharpens an image. We convolve the image with a Gaussian operator of the given radius and standard deviation \(sigma). For reasonable results, the radius should be larger than sigma. Use a radius of 0 and SharpenImage\() selects a suitable radius for you.

The format of the MagickSharpenImage method is:
  unsigned int MagickSharpenImage\( MagickWand *wand, const double radius, const double sigma );
wand:
  The magick wand.
radius:
  The radius of the Gaussian, in pixels, not counting the center
pixel.
sigma:
  The standard deviation of the Gaussian, in pixels.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (radius %double)
  (sigma %double))

(defcfun ("MagickShaveImage" %MagickShaveImage)
    %magick-status
  "Shaves pixels from the image edges. It allocates the memory necessary for the new Image structure and returns a pointer to the new image.

The format of the MagickShaveImage method is:
  unsigned int MagickShaveImage\( MagickWand *wand, const unsigned long columns,
                               const unsigned long rows );
wand:
  The magick wand.
columns:
  The number of columns in the scaled image.
rows:
  The number of rows in the scaled image.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (columns :unsigned-long)
  (rows :unsigned-long))

(defcfun ("MagickShearImage" %MagickShearImage)
    %magick-status
  "Slides one edge of an image along the X or Y axis, creating a parallelogram. An X direction shear slides an edge along the X axis, while a Y direction shear slides an edge along the Y axis. The amount of the shear is controlled by a shear angle. For X direction shears, x_shear is measured relative to the Y axis, and similarly, for Y direction shears y_shear is measured relative to the X axis. Empty triangles left over from shearing the image are filled with the background color.

The format of the MagickShearImage method is:
  unsigned int MagickShearImage\( MagickWand *wand, const PixelWand *background,
                               const double x_shear, onst double y_shear );
wand:
  The magick wand.
background:
  The background pixel wand.
x_shear:
  The number of degrees to shear the image.
y_shear:
  The number of degrees to shear the image.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (background %PixelWand)
  (x_shear %double)
  (y_shear %double))

(defcfun ("MagickSolarizeImage" %MagickSolarizeImage)
    %magick-status
  "Applies a special effect to the image, similar to the effect achieved in a photo darkroom by selectively exposing areas of photo sensitive paper to light. Threshold ranges from 0 to MaxRGB and is a measure of the extent of the solarization.

The format of the MagickSolarizeImage method is:
  unsigned int MagickSolarizeImage\( MagickWand *wand, const double threshold );

A description of each parameter follows:
wand:
  The magick wand.
threshold:
  Define the extent of the solarization.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (threshold %double))

(defcfun ("MagickSpreadImage" %MagickSpreadImage)
    %magick-status
  "Is a special effects method that randomly displaces each pixel in a block defined by the radius parameter.

The format of the MagickSpreadImage method is:
  unsigned int MagickSpreadImage\( MagickWand *wand, const double radius );

A description of each parameter follows:
wand:
  The magick wand.
radius:
  Choose a random pixel in a neighborhood of this extent.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (radius %double))

(defcfun ("MagickSteganoImage" %MagickSteganoImage)
    %MagickWand
  "Use MagickSteganoImage\() to hide a digital watermark within the image. Recover the hidden watermark later to prove that the authenticity of an image. Offset defines the start position within the image to hide the watermark.

The format of the MagickSteganoImage method is:
  MagickWand *MagickSteganoImage\( MagickWand *wand, const MagickWand *watermark_wand,
                                const long offset );
wand:
  The magick wand.
watermark_wand:
  The watermark wand.
offset:
  Start hiding at this offset into the image.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (watermark_wand %MagickWand)
  (offset :long))

(defcfun ("MagickStereoImage" %MagickStereoImage)
    %MagickWand
  "Composites two images and produces a single image that is the composite of a left and right image of a stereo pair

The format of the MagickStereoImage method is:
  MagickWand *MagickStereoImage\( MagickWand *wand, const MagickWand *offset_wand );
wand:
  The magick wand.
offset_wand:
  Another image wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (offset_wand %MagickWand))

(defcfun ("MagickStripImage" %MagickStripImage)
    %magick-status
  "Removes all profiles and text attributes from the image.

The format of the MagickStripImage method is:
  unsigned int MagickStripImage\( MagickWand *wand );

A description of each parameter follows:
wand:
  The magick wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand))

(defcfun ("MagickSwirlImage" %MagickSwirlImage)
    %magick-status
  "Swirls the pixels about the center of the image, where degrees indicates the sweep of the arc through which each pixel is moved. You get a more dramatic effect as the degrees move from 1 to 360.

The format of the MagickSwirlImage method is:
  unsigned int MagickSwirlImage\( MagickWand *wand, const double degrees );

A description of each parameter follows:
wand:
  The magick wand.
degrees:
  Define the tightness of the swirling effect.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (degrees %double))

(defcfun ("MagickTextureImage" %MagickTextureImage)
    %MagickWand
  "Repeatedly tiles the texture image across and down the image canvas.

The format of the MagickTextureImage method is:
  MagickWand *MagickTextureImage\( MagickWand *wand, const MagickWand *texture_wand );
wand:
  The magick wand.
texture_wand:
  The texture wand

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (texture_wand %MagickWand))

(defcfun ("MagickThresholdImage" %MagickThresholdImage)
    %magick-status
  "Changes the value of individual pixels based on the intensity of each pixel compared to threshold. The result is a high-contrast, two color image.

The format of the MagickThresholdImage method is:
  unsigned int MagickThresholdImage\( MagickWand *wand, const double threshold );
wand:
  The magick wand.
threshold:
  Define the threshold value.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (threshold %double))

(defcfun ("MagickThresholdImageChannel" %MagickThresholdImageChannel)
    %magick-status
  "Changes the value of individual pixel component based on the intensity of each pixel compared to threshold. The result is a high-contrast, two color image.

The format of the MagickThresholdImage method is:
  unsigned int MagickThresholdImageChannel\( MagickWand *wand, const ChannelType channel,
                                          const double threshold );
wand:
  The magick wand.
channel:
  The channel.
threshold:
  Define the threshold value.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (channel %ChannelType)
  (threshold %double))

(defcfun ("MagickTintImage" %MagickTintImage)
    %magick-status
  "Applies a color vector to each pixel in the image. The length of the vector is 0 for black and white and at its maximum for the midtones. The vector weighting function is f\(x)=\(1-\(4.0*\(\(x-0.5)*\(x-0.5)))).

The format of the MagickTintImage method is:
  unsigned int MagickTintImage\( MagickWand *wand, const PixelWand *tint,
                              const PixelWand *opacity );
wand:
  The magick wand.
tint:
  The tint pixel wand.
opacity:
  The opacity pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (tint %PixelWand)
  (opacity %PixelWand))

(defcfun ("MagickTransformImage" %MagickTransformImage)
    %MagickWand
  "Is a convenience method that behaves like MagickResizeImage\() or MagickCropImage\() but accepts scaling and/or cropping information as a region geometry specification. If the operation fails, the original image handle is returned.

The format of the MagickTransformImage method is:
  MagickWand *MagickTransformImage\( MagickWand *wand, const char *crop,
                                  const char *geometry );
wand:
  The magick wand.
crop:
  A crop geometry string.  This geometry defines a subregion of the
image to crop.
geometry:
  An image geometry string.  This geometry defines the final
size of the image.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (crop :string)
  (geometry :string))

(defcfun ("MagickTransparentImage" %MagickTransparentImage)
    %magick-status
  "Changes any pixel that matches color with the color defined by fill.

The format of the MagickTransparentImage method is:
  unsigned int MagickTransparentImage\( MagickWand *wand, const PixelWand *target,
                                     const unsigned int opacity, const double fuzz );
wand:
  The magick wand.
target:
  Change this target color to specified opacity value within
the image.
opacity:
  The replacement opacity value.
fuzz:
  By default target must match a particular pixel color
exactly.  However, in many cases two colors may differ by a small amount.
The fuzz member of image defines how much tolerance is acceptable to
consider two colors as the same.  For example, set fuzz to 10 and the
color red at intensities of 100 and 102 respectively are now interpreted
as the same color for the purposes of the floodfill.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (target %PixelWand)
  (opacity :unsigned-int)
  (fuzz %double))

(defcfun ("MagickTrimImage" %MagickTrimImage)
    %magick-status
  "Remove edges that are the background color from the image.

The format of the MagickTrimImage method is:
  unsigned int MagickTrimImage\( MagickWand *wand, const double fuzz );

A description of each parameter follows:
wand:
  The magick wand.
fuzz:
  By default target must match a particular pixel color
exactly.  However, in many cases two colors may differ by a small amount.
The fuzz member of image defines how much tolerance is acceptable to
consider two colors as the same.  For example, set fuzz to 10 and the
color red at intensities of 100 and 102 respectively are now interpreted
as the same color for the purposes of the floodfill.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (fuzz %double))

(defcfun ("MagickUnsharpMaskImage" %MagickUnsharpMaskImage)
    %magick-status
  "Sharpens an image. We convolve the image with a Gaussian operator of the given radius and standard deviation \(sigma). For reasonable results, radius should be larger than sigma. Use a radius of 0 and UnsharpMaskImage\() selects a suitable radius for you.

The format of the MagickUnsharpMaskImage method is:
  unsigned int MagickUnsharpMaskImage\( MagickWand *wand, const double radius, const double sigma,
                                     const double amount, const double threshold );
wand:
  The magick wand.
radius:
  The radius of the Gaussian, in pixels, not counting the center
pixel.
sigma:
  The standard deviation of the Gaussian, in pixels.
amount:
  The percentage of the difference between the original and the
blur image that is added back into the original.
threshold:
  The threshold in pixels needed to apply the diffence amount.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (radius %double)
  (sigma %double)
  (amount %double)
  (threshold %double))

(defcfun ("MagickWaveImage" %MagickWaveImage)
    %magick-status
  "Creates a \"ripple\" effect in the image by shifting the pixels vertically along a sine wave whose amplitude and wavelength is specified by the given parameters.

The format of the MagickWaveImage method is:
  unsigned int MagickWaveImage\( MagickWand *wand, const double amplitude,
                              const double wave_length );
wand:
  The magick wand.
amplitude, wave_length:
  Define the amplitude and wave length of the
sine wave.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (amplitude %double)
  (wave_length %double))

(defcfun ("MagickWhiteThresholdImage" %MagickWhiteThresholdImage)
    %magick-status
  "Is like ThresholdImage\() but forces all pixels above the threshold into white while leaving all pixels below the threshold unchanged.

The format of the MagickWhiteThresholdImage method is:
  unsigned int MagickWhiteThresholdImage\( MagickWand *wand, const PixelWand *threshold );
wand:
  The magick wand.
threshold:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (threshold %PixelWand))

(defcfun ("MagickWriteImage" %MagickWriteImage)
    %magick-status
  "Writes an image.

The format of the MagickWriteImage method is:
  unsigned int MagickWriteImage\( MagickWand *wand, const char *filename );

A description of each parameter follows:
wand:
  The magick wand.
filename:
  The image filename.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (filename :string))

(defcfun ("MagickWriteImagesFile" %MagickWriteImagesFile)
    %magick-status
  "Writes an image or image sequence to a stdio FILE handle. This may be used to append an encoded image to an already existing appended image sequence if the file seek position is at the end of an existing file.

The format of the MagickWriteImages method is:
  unsigned int MagickWriteImagesFile\( MagickWand *wand, FILE *file, const unsigned int adjoin );
wand:
  The magick wand.
file:
  The open \(and positioned) file handle.
adjoin:
  join images into a single multi-image file.

Since GraphicsMagick v1.3.13"
  (wand %MagickWand)
  (file %FILE)
  (adjoin :unsigned-int))

(defcfun ("MagickWriteImageBlob" %MagickWriteImageBlob)
    :pointer
  "Implements direct to memory image formats. It returns the image as a blob \(a formatted \"file\" in memory) and its length, starting from the current position in the image sequence. Use MagickSetImageFormat\() to set the format to write to the blob \(GIF, JPEG, PNG, etc.).

Use MagickResetIterator\() on the wand if it is desired to write
a sequence from the beginning and the iterator is not currently
at the beginning.

The format of the MagickWriteImageBlob method is:
  unsigned char *MagickWriteImageBlob\( MagickWand *wand, size_t *length );

A description of each parameter follows:
wand:
  The magick wand.
length:
  The length of the blob.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (length :pointer))

(defcfun ("MagickWriteImageFile" %MagickWriteImageFile)
    %magick-status
  "Writes an image to an open file descriptor.

The format of the MagickWandToFile method is:
  unsigned int MagickWriteImageFile\( MagickWand *wand, FILE *file );

A description of each parameter follows:
wand:
  The magick wand.
file:
  The file descriptor.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (file %FILE))

(defcfun ("MagickWriteImages" %MagickWriteImages)
    %magick-status
  "Writes an image or image sequence. If the wand represents an image sequence, then it is written starting at the first frame in the sequence.

The format of the MagickWriteImages method is:
  unsigned int MagickWriteImages\( MagickWand *wand, const char *filename,
                                const unsigned int adjoin );
wand:
  The magick wand.
filename:
  The image filename.
adjoin:
  join images into a single multi-image file.

Since GraphicsMagick v1.1.0"
  (wand %MagickWand)
  (filename :string)
  (adjoin :unsigned-int))

(defcfun ("NewMagickWand" %NewMagickWand)
    %MagickWand
  "Returns a wand required for all other methods in the API.

The format of the NewMagickWand method is:
  MagickWand NewMagickWand\( void );

Since GraphicsMagick v1.1.0")
