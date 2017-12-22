(in-package :gm)

(defcfun ("MagickCloneDrawingWand" %MagickCloneDrawingWand)
    %DrawingWand
  "Returns a new drawing wand which is a full \(deep) copy of an existing drawing wand.

The format of the  CloneDrawingWand method is:
  DrawingWand *CloneDrawingWand\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand to copy

Since GraphicsMagick v1.3.7"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDestroyDrawingWand" %MagickDestroyDrawingWand)
    :void
  "Frees all resources associated with the drawing wand. Once the drawing wand has been freed, it should not be used any further unless it re-allocated.

The format of the  DestroyDrawingWand method is:
  void DestroyDrawingWand\( DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand to destroy.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawAnnotation" %MagickDrawAnnotation)
    :void
  "Draws text on the image.

The format of the DrawAnnotation method is:
  void DrawAnnotation\( DrawingWand *drawing_wand, const double x, const double y,
                     const unsigned char *text );
drawing_wand:
  The drawing wand.
x:
  x ordinate to left of text
y:
  y ordinate to text baseline
text:
  text to draw

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x %double)
  (y %double)
  (text :string))

(defcfun ("MagickDrawAffine" %MagickDrawAffine)
    :void
  "Adjusts the current affine transformation matrix with the specified affine transformation matrix. Note that the current affine transform is adjusted rather than replaced.

The format of the DrawAffine method is:
  void DrawAffine\( DrawingWand *drawing_wand, const AffineMatrix *affine );

A description of each parameter follows:
drawing_wand:
  Drawing drawing_wand
affine:
  Affine matrix parameters

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (affine %AffineMatrix))

(defcfun ("MagickDrawAllocateWand" %MagickDrawAllocateWand)
    %DrawingWand
  "Allocates an initial drawing wand which is an opaque handle required by the remaining drawing methods.

The format of the DrawAllocateWand method is:
  DrawingWand DrawAllocateWand\( const DrawInfo *draw_info, Image *image );

A description of each parameter follows:
draw_info:
  Initial drawing defaults. Set to NULL to use
ImageMagick defaults.
image:
  The image to draw on.

Since GraphicsMagick v1.1.0
Deprecated since GraphicsMagick v1.3.7"
  (draw_info %DrawInfo)
  (image %Image))

(defcfun ("MagickDrawArc" %MagickDrawArc)
    :void
  "Draws an arc falling within a specified bounding rectangle on the image.

The format of the DrawArc method is:
  void DrawArc\( DrawingWand *drawing_wand, const double sx, const double sy, const double ex,
              const double ey, const double sd, const double ed );
drawing_wand:
  The drawing wand.
sx:
  starting x ordinate of bounding rectangle
sy:
  starting y ordinate of bounding rectangle
ex:
  ending x ordinate of bounding rectangle
ey:
  ending y ordinate of bounding rectangle
sd:
  starting degrees of rotation
ed:
  ending degrees of rotation

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (sx %double)
  (sy %double)
  (ex %double)
  (ey %double)
  (sd %double)
  (ed %double))

(defcfun ("MagickDrawBezier" %MagickDrawBezier)
    :void
  "Draws a bezier curve through a set of points on the image.

The format of the DrawBezier method is:
  void DrawBezier\( DrawingWand *drawing_wand, const unsigned long number_coordinates,
                 const PointInfo *coordinates );
drawing_wand:
  The drawing wand.
number_coordinates:
  number of coordinates
coordinates:
  coordinates

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (number_coordinates :unsigned-long)
  (coordinates %PointInfo))

(defcfun ("MagickDrawCircle" %MagickDrawCircle)
    :void
  "Draws a circle on the image.

The format of the DrawCircle method is:
  void DrawCircle\( DrawingWand *drawing_wand, const double ox, const double oy, const double px,
                 const double py );
drawing_wand:
  The drawing wand.
ox:
  origin x ordinate
oy:
  origin y ordinate
px:
  perimeter x ordinate
py:
  perimeter y ordinate

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (ox %double)
  (oy %double)
  (px %double)
  (py %double))

(defcfun ("MagickDrawClearException" %MagickDrawClearException)
    :unsigned-int
  "Clears any existing exception from the drawing wand.

The format of the DrawGetException method is:
  unsigned int DrawClearException\( DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.3.7"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawGetClipPath" %MagickDrawGetClipPath)
    :string
  "Obtains the current clipping path ID. The value returned must be deallocated by the user when it is no longer needed.

The format of the DrawGetClipPath method is:
  char *DrawGetClipPath\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetClipPath" %MagickDrawSetClipPath)
    :void
  "Associates a named clipping path with the image. Only the areas drawn on by the clipping path will be modified as long as it remains in effect.

The format of the DrawSetClipPath method is:
  void DrawSetClipPath\( DrawingWand *drawing_wand, const char *clip_path );

A description of each parameter follows:
drawing_wand:
  The drawing wand.
clip_path:
  name of clipping path to associate with image

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (clip_path :string))

(defcfun ("MagickDrawGetClipRule" %MagickDrawGetClipRule)
    %FillRule
  "Returns the current polygon fill rule to be used by the clipping path.

The format of the DrawGetClipRule method is:
  FillRule DrawGetClipRule\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetClipRule" %MagickDrawSetClipRule)
    :void
  "Set the polygon fill rule to be used by the clipping path.

The format of the DrawSetClipRule method is:
  void DrawSetClipRule\( DrawingWand *drawing_wand, const FillRule fill_rule );

A description of each parameter follows:
drawing_wand:
  The drawing wand.
fill_rule:
  fill rule \(EvenOddRule or NonZeroRule)

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (fill_rule %FillRule))

(defcfun ("MagickDrawGetClipUnits" %MagickDrawGetClipUnits)
    %ClipPathUnits
  "Returns the interpretation of clip path units.

The format of the DrawGetClipUnits method is:
  ClipPathUnits DrawGetClipUnits\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawGetException" %MagickDrawGetException)
    :string
  "Obtains error information associated with the last exception \(if any). If an exception did occur, an allocated text string is returned which contains a detailed description of the exception. This string must be deallocated by the user once it is no longer needed.

The format of the DrawGetException method is:
  char *DrawGetException\( const DrawingWand *drawing_wand, ExceptionType *severity );
drawing_wand:
  The drawing wand.
severity:
  Enumeration corresponding to last thrown exception.

Since GraphicsMagick v1.3.7"
  (drawing_wand %DrawingWand)
  (severity %ExceptionType))

(defcfun ("MagickDrawSetClipUnits" %MagickDrawSetClipUnits)
    :void
  "Sets the interpretation of clip path units.

The format of the DrawSetClipUnits method is:
  void DrawSetClipUnits\( DrawingWand *drawing_wand, const ClipPathUnits clip_units );
drawing_wand:
  The drawing wand.
clip_units:
  units to use \(UserSpace, UserSpaceOnUse, or ObjectBoundingBox)

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (clip_units %ClipPathUnits))

(defcfun ("MagickDrawColor" %MagickDrawColor)
    :void
  "Draws color on image using the current fill color, starting at specified position, and using specified paint method. The available paint methods are:

PointMethod: Recolors the target pixel
ReplaceMethod: Recolor any pixel that matches the target pixel.
FloodfillMethod: Recolors target pixels and matching neighbors.
FillToBorderMethod: Recolor target pixels and neighbors not matching
ResetMethod: Recolor all pixels.

The format of the DrawColor method is:
  void DrawColor\( DrawingWand *drawing_wand, const double x, const double y,
                const PaintMethod paintMethod );
drawing_wand:
  The drawing wand.
x:
  x ordinate
y:
  y ordinate
paintMethod:
  paint method

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x %double)
  (y %double)
  (paintMethod %PaintMethod))

(defcfun ("MagickDrawComment" %MagickDrawComment)
    :void
  "Adds a comment to a vector output stream.

The format of the DrawComment method is:
  void DrawComment\( DrawingWand *drawing_wand, const char *comment );

A description of each parameter follows:
drawing_wand:
  The drawing wand.
comment:
  comment text

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (comment :string))

(defcfun ("MagickDrawEllipse" %MagickDrawEllipse)
    :void
  "Draws an ellipse on the image.

The format of the DrawEllipse method is:
  void DrawEllipse\( DrawingWand *drawing_wand, const double ox, const double oy, const double rx,
                  const double ry, const double start, const double end );
drawing_wand:
  The drawing wand.
ox:
  origin x ordinate
oy:
  origin y ordinate
rx:
  radius in x
ry:
  radius in y
start:
  starting rotation in degrees
end:
  ending rotation in degrees

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (ox %double)
  (oy %double)
  (rx %double)
  (ry %double)
  (start %double)
  (end %double))

(defcfun ("MagickDrawGetFillColor" %MagickDrawGetFillColor)
    :void
  "Returns the fill color used for drawing filled objects.

The format of the DrawGetFillColor method is:
  void DrawGetFillColor\( const DrawingWand *drawing_wand, PixelWand *fill_color );
drawing_wand:
  The drawing wand.
fill_color:
  Return the fill color.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (fill_color %PixelWand))

(defcfun ("MagickDrawSetFillColor" %MagickDrawSetFillColor)
    :void
  "Sets the fill color to be used for drawing filled objects.

The format of the DrawSetFillColor method is:
  void DrawSetFillColor\( DrawingWand *drawing_wand, const PixelWand *fill_wand );
drawing_wand:
  The drawing wand.
fill_wand:
  fill wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (fill_wand %PixelWand))

(defcfun ("MagickDrawSetFillPatternURL" %MagickDrawSetFillPatternURL)
    :void
  "Sets the URL to use as a fill pattern for filling objects. Only local URLs \(\"#identifier\") are supported at this time. These local URLs are normally created by defining a named fill pattern with DrawPushPattern/DrawPopPattern.

The format of the DrawSetFillPatternURL method is:
  void DrawSetFillPatternURL\( DrawingWand *drawing_wand, const char *fill_url );
drawing_wand:
  The drawing wand.
fill_url:
  URL to use to obtain fill pattern.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (fill_url :string))

(defcfun ("MagickDrawGetFillOpacity" %MagickDrawGetFillOpacity)
    :double
  "Returns the opacity used when drawing using the fill color or fill texture. Fully opaque is 1.0.

The format of the DrawGetFillOpacity method is:
  double DrawGetFillOpacity\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetFillOpacity" %MagickDrawSetFillOpacity)
    :void
  "Sets the opacity to use when drawing using the fill color or fill texture. Fully opaque is 1.0.

The format of the DrawSetFillOpacity method is:
  void DrawSetFillOpacity\( DrawingWand *drawing_wand, const double fill_opacity );
drawing_wand:
  The drawing wand.
fill_opacity:
  fill opacity

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (fill_opacity %double))

(defcfun ("MagickDrawGetFillRule" %MagickDrawGetFillRule)
    %FillRule
  "Returns the fill rule used while drawing polygons.

The format of the DrawGetFillRule method is:
  FillRule DrawGetFillRule\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetFillRule" %MagickDrawSetFillRule)
    :void
  "Sets the fill rule to use while drawing polygons.

The format of the DrawSetFillRule method is:
  void DrawSetFillRule\( DrawingWand *drawing_wand, const FillRule fill_rule );

A description of each parameter follows:
drawing_wand:
  The drawing wand.
fill_rule:
  fill rule \(EvenOddRule or NonZeroRule)

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (fill_rule %FillRule))

(defcfun ("MagickDrawGetFont" %MagickDrawGetFont)
    :string
  "Returns a null-terminaged string specifying the font used when annotating with text. The value returned must be freed by the user when no longer needed.

The format of the DrawGetFont method is:
  char *DrawGetFont\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetFont" %MagickDrawSetFont)
    :void
  "Sets the fully-sepecified font to use when annotating with text.

The format of the DrawSetFont method is:
  void DrawSetFont\( DrawingWand *drawing_wand, const char *font_name );

A description of each parameter follows:
drawing_wand:
  The drawing wand.
font_name:
  font name

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (font_name :string))

(defcfun ("MagickDrawGetFontFamily" %MagickDrawGetFontFamily)
    :string
  "Returns the font family to use when annotating with text. The value returned must be freed by the user when it is no longer needed.

The format of the DrawGetFontFamily method is:
  char *DrawGetFontFamily\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetFontFamily" %MagickDrawSetFontFamily)
    :void
  "Sets the font family to use when annotating with text.

The format of the DrawSetFontFamily method is:
  void DrawSetFontFamily\( DrawingWand *drawing_wand, const char *font_family );

A description of each parameter follows:
drawing_wand:
  The drawing wand.
font_family:
  font family

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (font_family :string))

(defcfun ("MagickDrawGetFontSize" %MagickDrawGetFontSize)
    :double
  "Returns the font pointsize used when annotating with text.

The format of the DrawGetFontSize method is:
  double DrawGetFontSize\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetFontSize" %MagickDrawSetFontSize)
    :void
  "Sets the font pointsize to use when annotating with text.

The format of the DrawSetFontSize method is:
  void DrawSetFontSize\( DrawingWand *drawing_wand, const double pointsize );

A description of each parameter follows:
drawing_wand:
  The drawing wand.
pointsize:
  text pointsize

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (pointsize %double))

(defcfun ("MagickDrawGetFontStretch" %MagickDrawGetFontStretch)
    %StretchType
  "Returns the font stretch used when annotating with text.

The format of the DrawGetFontStretch method is:
  StretchType DrawGetFontStretch\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetFontStretch" %MagickDrawSetFontStretch)
    :void
  "Sets the font stretch to use when annotating with text. The AnyStretch enumeration acts as a wild-card \"don't care\" option.

The format of the DrawSetFontStretch method is:
  void DrawSetFontStretch\( DrawingWand *drawing_wand, const StretchType font_stretch );
drawing_wand:
  The drawing wand.
font_stretch:
  font stretch \(NormalStretch, UltraCondensedStretch,
CondensedStretch, SemiCondensedStretch,
SemiExpandedStretch, ExpandedStretch,
ExtraExpandedStretch, UltraExpandedStretch, AnyStretch)

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (font_stretch %StretchType))

(defcfun ("MagickDrawGetFontStyle" %MagickDrawGetFontStyle)
    %StyleType
  "Returns the font style used when annotating with text.

The format of the DrawGetFontStyle method is:
  StyleType DrawGetFontStyle\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetFontStyle" %MagickDrawSetFontStyle)
    :void
  "Sets the font style to use when annotating with text. The AnyStyle enumeration acts as a wild-card \"don't care\" option.

The format of the DrawSetFontStyle method is:
  void DrawSetFontStyle\( DrawingWand *drawing_wand, const StyleType style );

A description of each parameter follows:
drawing_wand:
  The drawing wand.
style:
  font style \(NormalStyle, ItalicStyle, ObliqueStyle, AnyStyle)

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (style %StyleType))

(defcfun ("MagickDrawGetFontWeight" %MagickDrawGetFontWeight)
    :unsigned-long
  "Returns the font weight used when annotating with text.

The format of the DrawGetFontWeight method is:
  unsigned long DrawGetFontWeight\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetFontWeight" %MagickDrawSetFontWeight)
    :void
  "Sets the font weight to use when annotating with text.

The format of the DrawSetFontWeight method is:
  void DrawSetFontWeight\( DrawingWand *drawing_wand, const unsigned long font_weight );
drawing_wand:
  The drawing wand.
font_weight:
  font weight \(valid range 100-900)

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (font_weight :unsigned-long))

(defcfun ("MagickDrawGetGravity" %MagickDrawGetGravity)
    %GravityType
  "Returns the text placement gravity used when annotating with text.

The format of the DrawGetGravity method is:
  GravityType DrawGetGravity\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetGravity" %MagickDrawSetGravity)
    :void
  "Sets the text placement gravity to use when annotating with text.

The format of the DrawSetGravity method is:
  void DrawSetGravity\( DrawingWand *drawing_wand, const GravityType gravity );

A description of each parameter follows:
drawing_wand:
  The drawing wand.
gravity:
  positioning gravity \(NorthWestGravity, NorthGravity,
NorthEastGravity, WestGravity, CenterGravity,
EastGravity, SouthWestGravity, SouthGravity,
SouthEastGravity)

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (gravity %GravityType))

(defcfun ("MagickDrawComposite" %MagickDrawComposite)
    :void
  "Composites an image onto the current image, using the specified composition operator, specified position, and at the specified size.

The format of the DrawComposite method is:
  void DrawComposite\( DrawingWand *drawing_wand, const CompositeOperator composite_operator,
                    const double x, const double y, const double width, const double height,
                    const Image *image );
drawing_wand:
  The drawing wand.
composite_operator:
  composition operator
x:
  x ordinate of top left corner
y:
  y ordinate of top left corner
width:
  Width to resize image to prior to compositing.  Specify zero to
use existing width.
height:
  Height to resize image to prior to compositing.  Specify zero
to use existing height.
image:
  Image to composite

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (composite_operator %CompositeOperator)
  (x %double)
  (y %double)
  (width %double)
  (height %double)
  (image %Image))

(defcfun ("MagickDrawLine" %MagickDrawLine)
    :void
  "Draws a line on the image using the current stroke color, stroke opacity, and stroke width.

The format of the DrawLine method is:
  void DrawLine\( DrawingWand *drawing_wand, const double sx, const double sy, const double ex,
               const double ey );
drawing_wand:
  The drawing wand.
sx:
  starting x ordinate
sy:
  starting y ordinate
ex:
  ending x ordinate
ey:
  ending y ordinate

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (sx %double)
  (sy %double)
  (ex %double)
  (ey %double))

(defcfun ("MagickDrawMatte" %MagickDrawMatte)
    :void
  "Paints on the image's opacity channel in order to set effected pixels to transparent. to influence the opacity of pixels. The available paint methods are:

PointMethod: Select the target pixel
ReplaceMethod: Select any pixel that matches the target pixel.
FloodfillMethod: Select the target pixel and matching neighbors.
FillToBorderMethod: Select the target pixel and neighbors not matching
border color.
ResetMethod: Select all pixels.

The format of the DrawMatte method is:
  void DrawMatte\( DrawingWand *drawing_wand, const double x, const double y,
                const PaintMethod paint_method );
drawing_wand:
  The drawing wand.
x:
  x ordinate
y:
  y ordinate

o paint_method:

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x %double)
  (y %double)
  (paint_method %PaintMethod))

(defcfun ("MagickDrawPathClose" %MagickDrawPathClose)
    :void
  "Adds a path element to the current path which closes the current subpath by drawing a straight line from the current point to the current subpath's most recent starting point \(usually, the most recent moveto point).

The format of the DrawPathClose method is:
  void DrawPathClose\( DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawPathCurveToAbsolute" %MagickDrawPathCurveToAbsolute)
    :void
  "Draws a cubic Bezier curve from the current point to \(x,y) using \(x1,y1) as the control point at the beginning of the curve and \(x2,y2) as the control point at the end of the curve using absolute coordinates. At the end of the command, the new current point becomes the final \(x,y) coordinate pair used in the polybezier.

The format of the DrawPathCurveToAbsolute method is:
  void DrawPathCurveToAbsolute\( DrawingWand *drawing_wand, const double x1, const double y1,
                              const double x2, const double y2, const double x,
                              const double y );
drawing_wand:
  The drawing wand.
x1:
  x ordinate of control point for curve beginning
y1:
  y ordinate of control point for curve beginning
x2:
  x ordinate of control point for curve ending
y2:
  y ordinate of control point for curve ending
x:
  x ordinate of the end of the curve
y:
  y ordinate of the end of the curve

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x1 %double)
  (y1 %double)
  (x2 %double)
  (y2 %double)
  (x %double)
  (y %double))

(defcfun ("MagickDrawPathCurveToRelative" %MagickDrawPathCurveToRelative)
    :void
  "Draws a cubic Bezier curve from the current point to \(x,y) using \(x1,y1) as the control point at the beginning of the curve and \(x2,y2) as the control point at the end of the curve using relative coordinates. At the end of the command, the new current point becomes the final \(x,y) coordinate pair used in the polybezier.

The format of the DrawPathCurveToRelative method is:
  void DrawPathCurveToRelative\( DrawingWand *drawing_wand, const double x1, const double y1,
                              const double x2, const double y2, const double x,
                              const double y );
drawing_wand:
  The drawing wand.
x1:
  x ordinate of control point for curve beginning
y1:
  y ordinate of control point for curve beginning
x2:
  x ordinate of control point for curve ending
y2:
  y ordinate of control point for curve ending
x:
  x ordinate of the end of the curve
y:
  y ordinate of the end of the curve

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x1 %double)
  (y1 %double)
  (x2 %double)
  (y2 %double)
  (x %double)
  (y %double))

(defcfun ("MagickDrawPathCurveToQuadraticBezierAbsolute" %MagickDrawPathCurveToQuadraticBezierAbsolute)
    :void
  "Draws a quadratic Bezier curve from the current point to \(x,y) using \(x1,y1) as the control point using absolute coordinates. At the end of the command, the new current point becomes the final \(x,y) coordinate pair used in the polybezier.

The format of the DrawPathCurveToQuadraticBezierAbsolute method is:
  void DrawPathCurveToQuadraticBezierAbsolute\( DrawingWand *drawing_wand, const double x1,
                                             const double y1, onst double x, const double y );
drawing_wand:
  The drawing wand.
x1:
  x ordinate of the control point
y1:
  y ordinate of the control point
x:
  x ordinate of final point
y:
  y ordinate of final point

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x1 %double)
  (y1 %double)
  (x %double)
  (y %double))

(defcfun ("MagickDrawPathCurveToQuadraticBezierRelative" %MagickDrawPathCurveToQuadraticBezierRelative)
    :void
  "Draws a quadratic Bezier curve from the current point to \(x,y) using \(x1,y1) as the control point using relative coordinates. At the end of the command, the new current point becomes the final \(x,y) coordinate pair used in the polybezier.

The format of the DrawPathCurveToQuadraticBezierRelative method is:
  void DrawPathCurveToQuadraticBezierRelative\( DrawingWand *drawing_wand, const double x1,
                                             const double y1, const double x,
                                             const double y );
drawing_wand:
  The drawing wand.
x1:
  x ordinate of the control point
y1:
  y ordinate of the control point
x:
  x ordinate of final point
y:
  y ordinate of final point

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x1 %double)
  (y1 %double)
  (x %double)
  (y %double))

(defcfun ("MagickDrawPathCurveToQuadraticBezierSmoothAbsolute" %MagickDrawPathCurveToQuadraticBezierSmoothAbsolute)
    :void
  "Draws a quadratic Bezier curve \(using absolute coordinates) from the current point to \(x,y). The control point is assumed to be the reflection of the control point on the previous command relative to the current point. \(If there is no previous command or if the previous command was not a DrawPathCurveToQuadraticBezierAbsolute, DrawPathCurveToQuadraticBezierRelative, DrawPathCurveToQuadraticBezierSmoothAbsolute or DrawPathCurveToQuadraticBezierSmoothRelative, assume the control point is coincident with the current point.). At the end of the command, the new current point becomes the final \(x,y) coordinate pair used in the polybezier.

The format of the DrawPathCurveToQuadraticBezierSmoothAbsolute method is:
  void DrawPathCurveToQuadraticBezierSmoothAbsolute\( DrawingWand *drawing_wand, const double x,
                                                   const double y );
drawing_wand:
  The drawing wand.
x:
  x ordinate of final point
y:
  y ordinate of final point

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x %double)
  (y %double))

(defcfun ("MagickDrawPathCurveToQuadraticBezierSmoothRelative" %MagickDrawPathCurveToQuadraticBezierSmoothRelative)
    :void
  "Draws a quadratic Bezier curve \(using relative coordinates) from the current point to \(x,y). The control point is assumed to be the reflection of the control point on the previous command relative to the current point. \(If there is no previous command or if the previous command was not a DrawPathCurveToQuadraticBezierAbsolute, DrawPathCurveToQuadraticBezierRelative, DrawPathCurveToQuadraticBezierSmoothAbsolute or DrawPathCurveToQuadraticBezierSmoothRelative, assume the control point is coincident with the current point.). At the end of the command, the new current point becomes the final \(x,y) coordinate pair used in the polybezier.

The format of the DrawPathCurveToQuadraticBezierSmoothRelative method is:
  void DrawPathCurveToQuadraticBezierSmoothRelative\( DrawingWand *drawing_wand, const double x,
                                                   const double y );
drawing_wand:
  The drawing wand.
x:
  x ordinate of final point
y:
  y ordinate of final point

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x %double)
  (y %double))

(defcfun ("MagickDrawPathCurveToSmoothAbsolute" %MagickDrawPathCurveToSmoothAbsolute)
    :void
  "Draws a cubic Bezier curve from the current point to \(x,y) using absolute coordinates. The first control point is assumed to be the reflection of the second control point on the previous command relative to the current point. \(If there is no previous command or if the previous command was not an DrawPathCurveToAbsolute, DrawPathCurveToRelative, DrawPathCurveToSmoothAbsolute or DrawPathCurveToSmoothRelative, assume the first control point is coincident with the current point.) \(x2,y2) is the second control point \(i.e., the control point at the end of the curve). At the end of the command, the new current point becomes the final \(x,y) coordinate pair used in the polybezier.

The format of the DrawPathCurveToSmoothAbsolute method is:
  void DrawPathCurveToSmoothAbsolute\( DrawingWand *drawing_wand, const double x2const double y2,
                                    const double x, const double y );
drawing_wand:
  The drawing wand.
x2:
  x ordinate of second control point
y2:
  y ordinate of second control point
x:
  x ordinate of termination point
y:
  y ordinate of termination point

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (y2 %double)
  (x %double)
  (y %double))

(defcfun ("MagickDrawPathCurveToSmoothRelative" %MagickDrawPathCurveToSmoothRelative)
    :void
  "Draws a cubic Bezier curve from the current point to \(x,y) using relative coordinates. The first control point is assumed to be the reflection of the second control point on the previous command relative to the current point. \(If there is no previous command or if the previous command was not an DrawPathCurveToAbsolute, DrawPathCurveToRelative, DrawPathCurveToSmoothAbsolute or DrawPathCurveToSmoothRelative, assume the first control point is coincident with the current point.) \(x2,y2) is the second control point \(i.e., the control point at the end of the curve). At the end of the command, the new current point becomes the final \(x,y) coordinate pair used in the polybezier.

The format of the DrawPathCurveToSmoothRelative method is:
  void DrawPathCurveToSmoothRelative\( DrawingWand *drawing_wand, const double x2,
                                    const double y2, const double x, const double y );
drawing_wand:
  The drawing wand.
x2:
  x ordinate of second control point
y2:
  y ordinate of second control point
x:
  x ordinate of termination point
y:
  y ordinate of termination point

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x2 %double)
  (y2 %double)
  (x %double)
  (y %double))

(defcfun ("MagickDrawPathEllipticArcAbsolute" %MagickDrawPathEllipticArcAbsolute)
    :void
  "Draws an elliptical arc from the current point to \(x, y) using absolute coordinates. The size and orientation of the ellipse are defined by two radii \(rx, ry) and an xAxisRotation, which indicates how the ellipse as a whole is rotated relative to the current coordinate system. The center \(cx, cy) of the ellipse is calculated automatically to satisfy the constraints imposed by the other parameters. largeArcFlag and sweepFlag contribute to the automatic calculations and help determine how the arc is drawn. If largeArcFlag is true then draw the larger of the available arcs. If sweepFlag is true, then draw the arc matching a clock-wise rotation.

The format of the DrawPathEllipticArcAbsolute method is:
  void DrawPathEllipticArcAbsolute\( DrawingWand *drawing_wand, const double rx, const double ry,
                                  const double x_axis_rotation,
                                  unsigned int large_arc_flag, unsigned int sweep_flag,
                                  const double x, const double y );
drawing_wand:
  The drawing wand.
rx:
  x radius
ry:
  y radius
x_axis_rotation:
  indicates how the ellipse as a whole is rotated
relative to the current coordinate system
large_arc_flag:
  If non-zero \(true) then draw the larger of the
available arcs
sweep_flag:
  If non-zero \(true) then draw the arc matching a
clock-wise rotation

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (rx %double)
  (ry %double)
  (x_axis_rotation %double)
  (large_arc_flag :unsigned-int)
  (sweep_flag :unsigned-int)
  (x %double)
  (y %double))

(defcfun ("MagickDrawPathEllipticArcRelative" %MagickDrawPathEllipticArcRelative)
    :void
  "Draws an elliptical arc from the current point to \(x, y) using relative coordinates. The size and orientation of the ellipse are defined by two radii \(rx, ry) and an xAxisRotation, which indicates how the ellipse as a whole is rotated relative to the current coordinate system. The center \(cx, cy) of the ellipse is calculated automatically to satisfy the constraints imposed by the other parameters. largeArcFlag and sweepFlag contribute to the automatic calculations and help determine how the arc is drawn. If largeArcFlag is true then draw the larger of the available arcs. If sweepFlag is true, then draw the arc matching a clock-wise rotation.

The format of the DrawPathEllipticArcRelative method is:
  void DrawPathEllipticArcRelative\( DrawingWand *drawing_wand, const double rx, const double ry,
                                  const double x_axis_rotation,
                                  unsigned int large_arc_flag, unsigned int sweep_flag,
                                  const double x, const double y );
drawing_wand:
  The drawing wand.
rx:
  x radius
ry:
  y radius
x_axis_rotation:
  indicates how the ellipse as a whole is rotated
relative to the current coordinate system
large_arc_flag:
  If non-zero \(true) then draw the larger of the
available arcs
sweep_flag:
  If non-zero \(true) then draw the arc matching a
clock-wise rotation

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (rx %double)
  (ry %double)
  (x_axis_rotation %double)
  (large_arc_flag :unsigned-int)
  (sweep_flag :unsigned-int)
  (x %double)
  (y %double))

(defcfun ("MagickDrawPathFinish" %MagickDrawPathFinish)
    :void
  "Terminates the current path.

The format of the DrawPathFinish method is:
  void DrawPathFinish\( DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawPathLineToAbsolute" %MagickDrawPathLineToAbsolute)
    :void
  "Draws a line path from the current point to the given coordinate using absolute coordinates. The coordinate then becomes the new current point.

The format of the DrawPathLineToAbsolute method is:
  void DrawPathLineToAbsolute\( DrawingWand *drawing_wand, const double x, const double y );

A description of each parameter follows:
drawing_wand:
  The drawing wand.
x:
  target x ordinate
y:
  target y ordinate

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x %double)
  (y %double))

(defcfun ("MagickDrawPathLineToRelative" %MagickDrawPathLineToRelative)
    :void
  "Draws a line path from the current point to the given coordinate using relative coordinates. The coordinate then becomes the new current point.

The format of the DrawPathLineToRelative method is:
  void DrawPathLineToRelative\( DrawingWand *drawing_wand, const double x, const double y );
drawing_wand:
  The drawing wand.
x:
  target x ordinate
y:
  target y ordinate

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x %double)
  (y %double))

(defcfun ("MagickDrawPathLineToHorizontalAbsolute" %MagickDrawPathLineToHorizontalAbsolute)
    :void
  "Draws a horizontal line path from the current point to the target point using absolute coordinates. The target point then becomes the new current point.

The format of the DrawPathLineToHorizontalAbsolute method is:
  void DrawPathLineToHorizontalAbsolute\( DrawingWand *drawing_wand, const PathMode mode,
                                       const double x );
drawing_wand:
  The drawing wand.
x:
  target x ordinate

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (mode %PathMode)
  (x %double))

(defcfun ("MagickDrawPathLineToHorizontalRelative" %MagickDrawPathLineToHorizontalRelative)
    :void
  "Draws a horizontal line path from the current point to the target point using relative coordinates. The target point then becomes the new current point.

The format of the DrawPathLineToHorizontalRelative method is:
  void DrawPathLineToHorizontalRelative\( DrawingWand *drawing_wand, const double x );
drawing_wand:
  The drawing wand.
x:
  target x ordinate

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x %double))

(defcfun ("MagickDrawPathLineToVerticalAbsolute" %MagickDrawPathLineToVerticalAbsolute)
    :void
  "Draws a vertical line path from the current point to the target point using absolute coordinates. The target point then becomes the new current point.

The format of the DrawPathLineToVerticalAbsolute method is:
  void DrawPathLineToVerticalAbsolute\( DrawingWand *drawing_wand, const double y );
drawing_wand:
  The drawing wand.
y:
  target y ordinate

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (y %double))

(defcfun ("MagickDrawPathLineToVerticalRelative" %MagickDrawPathLineToVerticalRelative)
    :void
  "Draws a vertical line path from the current point to the target point using relative coordinates. The target point then becomes the new current point.

The format of the DrawPathLineToVerticalRelative method is:
  void DrawPathLineToVerticalRelative\( DrawingWand *drawing_wand, const double y );
drawing_wand:
  The drawing wand.
y:
  target y ordinate

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (y %double))

(defcfun ("MagickDrawPathMoveToAbsolute" %MagickDrawPathMoveToAbsolute)
    :void
  "Starts a new sub-path at the given coordinate using absolute coordinates. The current point then becomes the specified coordinate.

The format of the DrawPathMoveToAbsolute method is:
  void DrawPathMoveToAbsolute\( DrawingWand *drawing_wand, const double x, const double y );
drawing_wand:
  The drawing wand.
x:
  target x ordinate
y:
  target y ordinate

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x %double)
  (y %double))

(defcfun ("MagickDrawPathMoveToRelative" %MagickDrawPathMoveToRelative)
    :void
  "Starts a new sub-path at the given coordinate using relative coordinates. The current point then becomes the specified coordinate.

The format of the DrawPathMoveToRelative method is:
  void DrawPathMoveToRelative\( DrawingWand *drawing_wand, const double x, const double y );
drawing_wand:
  The drawing wand.
x:
  target x ordinate
y:
  target y ordinate

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x %double)
  (y %double))

(defcfun ("MagickDrawPathStart" %MagickDrawPathStart)
    :void
  "Declares the start of a path drawing list which is terminated by a matching DrawPathFinish\() command. All other DrawPath commands must be enclosed between a DrawPathStart\() and a DrawPathFinish\() command. This is because path drawing commands are subordinate commands and they do not function by themselves.

The format of the DrawPathStart method is:
  void DrawPathStart\( DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawPeekGraphicContext" %MagickDrawPeekGraphicContext)
    %DrawInfo
  "Returns the current graphic drawing_wand.

The format of the DrawPeekGraphicContext method is:
  DrawInfo *DrawPeekGraphicContext\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawPoint" %MagickDrawPoint)
    :void
  "Draws a point using the current stroke color and stroke thickness at the specified coordinates.

The format of the DrawPoint method is:
  void DrawPoint\( DrawingWand *drawing_wand, const double x, const double y );

A description of each parameter follows:
drawing_wand:
  The drawing wand.
x:
  target x coordinate
y:
  target y coordinate

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x %double)
  (y %double))

(defcfun ("MagickDrawPolygon" %MagickDrawPolygon)
    :void
  "Draws a polygon using the current stroke, stroke width, and fill color or texture, using the specified array of coordinates.

The format of the DrawPolygon method is:
  void DrawPolygon\( DrawingWand *drawing_wand, const unsigned long number_coordinates,
                  const PointInfo *coordinates );
drawing_wand:
  The drawing wand.
number_coordinates:
  number of coordinates
coordinates:
  coordinate array

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (number_coordinates :unsigned-long)
  (coordinates %PointInfo))

(defcfun ("MagickDrawPolyline" %MagickDrawPolyline)
    :void
  "Draws a polyline using the current stroke, stroke width, and fill color or texture, using the specified array of coordinates.

The format of the DrawPolyline method is:
  void DrawPolyline\( DrawingWand *drawing_wand, const unsigned long number_coordinates,
                   const PointInfo *coordinates );
drawing_wand:
  The drawing wand.
number_coordinates:
  number of coordinates
coordinates:
  coordinate array

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (number_coordinates :unsigned-long)
  (coordinates %PointInfo))

(defcfun ("MagickDrawPopClipPath" %MagickDrawPopClipPath)
    :void
  "Terminates a clip path definition.

The format of the DrawPopClipPath method is:
  void DrawPopClipPath\( DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawPopDefs" %MagickDrawPopDefs)
    :void
  "Terminates a definition list

The format of the DrawPopDefs method is:
  void DrawPopDefs\( DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawPopGraphicContext" %MagickDrawPopGraphicContext)
    :void
  "Destroys the current drawing_wand returning to the previously pushed drawing wand. Multiple drawing wand may exist. It is an error to attempt to pop more drawing_wands than have been pushed, and it is proper form to pop all drawing_wands which have been pushed.

The format of the DrawPopGraphicContext method is:
  void DrawPopGraphicContext\( DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawPopPattern" %MagickDrawPopPattern)
    :void
  "Terminates a pattern definition.

The format of the DrawPopPattern method is:
  void DrawPopPattern\( DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawPushClipPath" %MagickDrawPushClipPath)
    :void
  "Starts a clip path definition which is comprized of any number of drawing commands and terminated by a DrawPopClipPath\() command.

The format of the DrawPushClipPath method is:
  void DrawPushClipPath\( DrawingWand *drawing_wand, const char *clip_path_id );

A description of each parameter follows:
drawing_wand:
  The drawing wand.
clip_path_id:
  string identifier to associate with the clip path for
later use.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (clip_path_id :string))

(defcfun ("MagickDrawPushDefs" %MagickDrawPushDefs)
    :void
  "Indicates that commands up to a terminating DrawPopDefs\() command create named elements \(e.g. clip-paths, textures, etc.) which may safely be processed earlier for the sake of efficiency.

The format of the DrawPushDefs method is:
  void DrawPushDefs\( DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawPushGraphicContext" %MagickDrawPushGraphicContext)
    :void
  "Clones the current drawing wand to create a new drawing wand. The original drawing drawing_wand\(s) may be returned to by invoking DrawPopGraphicContext\(). The drawing wands are stored on a drawing wand stack. For every Pop there must have already been an equivalent Push.

The format of the DrawPushGraphicContext method is:
  void DrawPushGraphicContext\( DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawPushPattern" %MagickDrawPushPattern)
    :void
  "Indicates that subsequent commands up to a DrawPopPattern\() command comprise the definition of a named pattern. The pattern space is assigned top left corner coordinates, a width and height, and becomes its own drawing space. Anything which can be drawn may be used in a pattern definition. Named patterns may be used as stroke or brush definitions.

The format of the DrawPushPattern method is:
  void DrawPushPattern\( DrawingWand *drawing_wand, const char *pattern_id, const double x,
                      const double y, const double width, const double height );
drawing_wand:
  The drawing wand.
pattern_id:
  pattern identification for later reference
x:
  x ordinate of top left corner
y:
  y ordinate of top left corner
width:
  width of pattern space
height:
  height of pattern space

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (pattern_id :string)
  (x %double)
  (y %double)
  (width %double)
  (height %double))

(defcfun ("MagickDrawRectangle" %MagickDrawRectangle)
    :void
  "Draws a rectangle given two coordinates and using the current stroke, stroke width, and fill settings.

The format of the DrawRectangle method is:
  void DrawRectangle\( DrawingWand *drawing_wand, const double x1, const double y1,
                    const double x2, const double y2 );
x1:
  x ordinate of first coordinate
y1:
  y ordinate of first coordinate
x2:
  x ordinate of second coordinate
y2:
  y ordinate of second coordinate

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x1 %double)
  (y1 %double)
  (x2 %double)
  (y2 %double))

(defcfun ("MagickDrawRender" %MagickDrawRender)
    :unsigned-int
  "Renders all preceding drawing commands onto the image. This function is deprecated. Use MagickDrawImage\() instead.

The format of the DrawRender method is:
  unsigned int DrawRender\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0
Deprecated since GraphicsMagick v1.3.7"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawRotate" %MagickDrawRotate)
    :void
  "Applies the specified rotation to the current coordinate space.

The format of the DrawRotate method is:
  void DrawRotate\( DrawingWand *drawing_wand, const double degrees );

A description of each parameter follows:
drawing_wand:
  The drawing wand.
degrees:
  degrees of rotation

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (degrees %double))

(defcfun ("MagickDrawRoundRectangle" %MagickDrawRoundRectangle)
    :void
  "Draws a rounted rectangle given two coordinates, x & y corner radiuses and using the current stroke, stroke width, and fill settings.

The format of the DrawRoundRectangle method is:
  void DrawRoundRectangle\( DrawingWand *drawing_wand, double x1, double y1, double x2, double y2,
                         double rx, double ry );
drawing_wand:
  The drawing wand.
x1:
  x ordinate of first coordinate
y1:
  y ordinate of first coordinate
x2:
  x ordinate of second coordinate
y2:
  y ordinate of second coordinate
rx:
  radius of corner in horizontal direction
ry:
  radius of corner in vertical direction

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x1 %double)
  (y1 %double)
  (x2 %double)
  (y2 %double)
  (rx %double)
  (ry %double))

(defcfun ("MagickDrawScale" %MagickDrawScale)
    :void
  "Adjusts the scaling factor to apply in the horizontal and vertical directions to the current coordinate space.

The format of the DrawScale method is:
  void DrawScale\( DrawingWand *drawing_wand, const double x, const double y );

A description of each parameter follows:
drawing_wand:
  The drawing wand.
x:
  horizontal scale factor
y:
  vertical scale factor

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x %double)
  (y %double))

(defcfun ("MagickDrawSkewX" %MagickDrawSkewX)
    :void
  "Skews the current coordinate system in the horizontal direction.

The format of the DrawSkewX method is:
  void DrawSkewX\( DrawingWand *drawing_wand, const double degrees );

A description of each parameter follows:
drawing_wand:
  The drawing wand.
degrees:
  number of degrees to skew the coordinates

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (degrees %double))

(defcfun ("MagickDrawSkewY" %MagickDrawSkewY)
    :void
  "Skews the current coordinate system in the vertical direction.

The format of the DrawSkewY method is:
  void DrawSkewY\( DrawingWand *drawing_wand, const double degrees );

A description of each parameter follows:
drawing_wand:
  The drawing wand.
degrees:
  number of degrees to skew the coordinates

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (degrees %double))

(defcfun ("MagickDrawGetStrokeColor" %MagickDrawGetStrokeColor)
    :void
  "Returns the color used for stroking object outlines.

The format of the DrawGetStrokeColor method is:
  void DrawGetStrokeColor\( const DrawingWand *drawing_wand, ;
drawing_wand:
  The drawing wand.
stroke_color:
  Return the stroke color.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (stroke_color %PixelWand))

(defcfun ("MagickDrawSetStrokeColor" %MagickDrawSetStrokeColor)
    :void
  "Sets the color used for stroking object outlines.

The format of the DrawSetStrokeColor method is:
  void DrawSetStrokeColor\( DrawingWand *drawing_wand, const PixelWand *stroke_wand );
drawing_wand:
  The drawing wand.
stroke_wand:
  stroke wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (stroke_wand %PixelWand))

(defcfun ("MagickDrawSetStrokePatternURL" %MagickDrawSetStrokePatternURL)
    :void
  "Sets the pattern used for stroking object outlines.

The format of the DrawSetStrokePatternURL method is:
  void DrawSetStrokePatternURL\( DrawingWand *drawing_wand, const char *stroke_url );
drawing_wand:
  The drawing wand.
stroke_url:
  URL specifying pattern ID \(e.g. \"#pattern_id\")

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (stroke_url :string))

(defcfun ("MagickDrawGetStrokeAntialias" %MagickDrawGetStrokeAntialias)
    :unsigned-int
  "Returns the current stroke antialias setting. Stroked outlines are antialiased by default. When antialiasing is disabled stroked pixels are thresholded to determine if the stroke color or underlying canvas color should be used.

The format of the DrawGetStrokeAntialias method is:
  unsigned int DrawGetStrokeAntialias\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetStrokeAntialias" %MagickDrawSetStrokeAntialias)
    :void
  "Controls whether stroked outlines are antialiased. Stroked outlines are antialiased by default. When antialiasing is disabled stroked pixels are thresholded to determine if the stroke color or underlying canvas color should be used.

The format of the DrawSetStrokeAntialias method is:
  void DrawSetStrokeAntialias\( DrawingWand *drawing_wand,
                             const unsigned int stroke_antialias );
drawing_wand:
  The drawing wand.
stroke_antialias:
  set to false \(zero) to disable antialiasing

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (stroke_antialias :unsigned-int))

(defcfun ("MagickDrawGetStrokeDashArray" %MagickDrawGetStrokeDashArray)
    :double
  "Returns an array representing the pattern of dashes and gaps used to stroke paths \(see DrawSetStrokeDashArray). The array must be freed once it is no longer required by the user.

The format of the DrawGetStrokeDashArray method is:
  double *DrawGetStrokeDashArray\( const DrawingWand *drawing_wand,
                                unsigned long *number_elements );
drawing_wand:
  The drawing wand.
number_elements:
  address to place number of elements in dash array

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (number_elements :pointer))

(defcfun ("MagickDrawSetStrokeDashArray" %MagickDrawSetStrokeDashArray)
    :void
  "Specifies the pattern of dashes and gaps used to stroke paths. The stroke dash array represents an array of numbers that specify the lengths of alternating dashes and gaps in pixels. If an odd number of values is provided, then the list of values is repeated to yield an even number of values. To remove an existing dash array, pass a zero number_elements argument and null dash_array. A typical stroke dash array might contain the members 5 3 2.

The format of the DrawSetStrokeDashArray method is:
  void DrawSetStrokeDashArray\( DrawingWand *drawing_wand, const unsigned long number_elements,
                             const double *dash_array );
drawing_wand:
  The drawing wand.
number_elements:
  number of elements in dash array
dash_array:
  dash array values

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (number_elements :unsigned-long)
  (dash_array :pointer))

(defcfun ("MagickDrawGetStrokeDashOffset" %MagickDrawGetStrokeDashOffset)
    :double
  "Returns the offset into the dash pattern to start the dash.

The format of the DrawGetStrokeDashOffset method is:
  double DrawGetStrokeDashOffset\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetStrokeDashOffset" %MagickDrawSetStrokeDashOffset)
    :void
  "Specifies the offset into the dash pattern to start the dash.

The format of the DrawSetStrokeDashOffset method is:
  void DrawSetStrokeDashOffset\( DrawingWand *drawing_wand, const double dash_offset );
drawing_wand:
  The drawing wand.
dash_offset:
  dash offset

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (dash_offset %double))

(defcfun ("MagickDrawGetStrokeLineCap" %MagickDrawGetStrokeLineCap)
    %LineCap
  "Returns the shape to be used at the end of open subpaths when they are stroked. Values of LineCap are UndefinedCap, ButtCap, RoundCap, and SquareCap.

The format of the DrawGetStrokeLineCap method is:
  LineCap DrawGetStrokeLineCap\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetStrokeLineCap" %MagickDrawSetStrokeLineCap)
    :void
  "Specifies the shape to be used at the end of open subpaths when they are stroked. Values of LineCap are UndefinedCap, ButtCap, RoundCap, and SquareCap.

The format of the DrawSetStrokeLineCap method is:
  void DrawSetStrokeLineCap\( DrawingWand *drawing_wand, const LineCap linecap );
drawing_wand:
  The drawing wand.
linecap:
  linecap style

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (linecap %LineCap))

(defcfun ("MagickDrawGetStrokeLineJoin" %MagickDrawGetStrokeLineJoin)
    %LineJoin
  "Returns the shape to be used at the corners of paths \(or other vector shapes) when they are stroked. Values of LineJoin are UndefinedJoin, MiterJoin, RoundJoin, and BevelJoin.

The format of the DrawGetStrokeLineJoin method is:
  LineJoin DrawGetStrokeLineJoin\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetStrokeLineJoin" %MagickDrawSetStrokeLineJoin)
    :void
  "Specifies the shape to be used at the corners of paths \(or other vector shapes) when they are stroked. Values of LineJoin are UndefinedJoin, MiterJoin, RoundJoin, and BevelJoin.

The format of the DrawSetStrokeLineJoin method is:
  void DrawSetStrokeLineJoin\( DrawingWand *drawing_wand, const LineJoin linejoin );
drawing_wand:
  The drawing wand.
linejoin:
  line join style

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (linejoin %LineJoin))

(defcfun ("MagickDrawGetStrokeMiterLimit" %MagickDrawGetStrokeMiterLimit)
    :unsigned-long
  "Returns the miter limit. When two line segments meet at a sharp angle and miter joins have been specified for 'lineJoin', it is possible for the miter to extend far beyond the thickness of the line stroking the path. The miterLimit' imposes a limit on the ratio of the miter length to the 'lineWidth'.

The format of the DrawGetStrokeMiterLimit method is:
  unsigned long DrawGetStrokeMiterLimit\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetStrokeMiterLimit" %MagickDrawSetStrokeMiterLimit)
    :void
  "Specifies the miter limit. When two line segments meet at a sharp angle and miter joins have been specified for 'lineJoin', it is possible for the miter to extend far beyond the thickness of the line stroking the path. The miterLimit' imposes a limit on the ratio of the miter length to the 'lineWidth'.

The format of the DrawSetStrokeMiterLimit method is:
  void DrawSetStrokeMiterLimit\( DrawingWand *drawing_wand, const unsigned long miterlimit );
drawing_wand:
  The drawing wand.
miterlimit:
  miter limit

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (miterlimit :unsigned-long))

(defcfun ("MagickDrawGetStrokeOpacity" %MagickDrawGetStrokeOpacity)
    :double
  "Returns the opacity of stroked object outlines.

The format of the DrawGetStrokeOpacity method is:
  double DrawGetStrokeOpacity\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetStrokeOpacity" %MagickDrawSetStrokeOpacity)
    :void
  "Specifies the opacity of stroked object outlines.

The format of the DrawSetStrokeOpacity method is:
  void DrawSetStrokeOpacity\( DrawingWand *drawing_wand, const double stroke_opacity );
drawing_wand:
  The drawing wand.
stroke_opacity:
  stroke opacity.  The value 1.0 is opaque.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (stroke_opacity %double))

(defcfun ("MagickDrawGetStrokeWidth" %MagickDrawGetStrokeWidth)
    :double
  "Returns the width of the stroke used to draw object outlines.

The format of the DrawGetStrokeWidth method is:
  double DrawGetStrokeWidth\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetStrokeWidth" %MagickDrawSetStrokeWidth)
    :void
  "Sets the width of the stroke used to draw object outlines.

The format of the DrawSetStrokeWidth method is:
  void DrawSetStrokeWidth\( DrawingWand *drawing_wand, const double stroke_width );
drawing_wand:
  The drawing wand.
stroke_width:
  stroke width

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (stroke_width %double))

(defcfun ("MagickDrawGetTextAntialias" %MagickDrawGetTextAntialias)
    :unsigned-int
  "Returns the current text antialias setting, which determines whether text is antialiased. Text is antialiased by default.

The format of the DrawGetTextAntialias method is:
  unsigned int DrawGetTextAntialias\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetTextAntialias" %MagickDrawSetTextAntialias)
    :void
  "Controls whether text is antialiased. Text is antialiased by default.

The format of the DrawSetTextAntialias method is:
  void DrawSetTextAntialias\( DrawingWand *drawing_wand, const unsigned int text_antialias );
drawing_wand:
  The drawing wand.
text_antialias:
  antialias boolean. Set to false \(0) to disable
antialiasing.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (text_antialias :unsigned-int))

(defcfun ("MagickDrawGetTextDecoration" %MagickDrawGetTextDecoration)
    %DecorationType
  "Returns the decoration applied when annotating with text.

The format of the DrawGetTextDecoration method is:
  DecorationType DrawGetTextDecoration\( DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetTextDecoration" %MagickDrawSetTextDecoration)
    :void
  "Specifies a decoration to be applied when annotating with text.

The format of the DrawSetTextDecoration method is:
  void DrawSetTextDecoration\( DrawingWand *drawing_wand, const DecorationType decoration );
drawing_wand:
  The drawing wand.
decoration:
  text decoration.  One of NoDecoration, UnderlineDecoration,
OverlineDecoration, or LineThroughDecoration

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (decoration %DecorationType))

(defcfun ("MagickDrawGetTextEncoding" %MagickDrawGetTextEncoding)
    :string
  "Returns a null-terminated string which specifies the code set used for text annotations. The string must be freed by the user once it is no longer required.

The format of the DrawGetTextEncoding method is:
  char *DrawGetTextEncoding\( const DrawingWand *drawing_wand );

A description of each parameter follows:
drawing_wand:
  The drawing wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand))

(defcfun ("MagickDrawSetTextEncoding" %MagickDrawSetTextEncoding)
    :void
  "Specifies specifies the code set to use for text annotations. The only character encoding which may be specified at this time is \"UTF-8\" for representing Unicode as a sequence of bytes. Specify an empty string to set text encoding to the system's default. Successful text annotation using Unicode may require fonts designed to support Unicode.

The format of the DrawSetTextEncoding method is:
  void DrawSetTextEncoding\( DrawingWand *drawing_wand, const char *encoding );

A description of each parameter follows:
drawing_wand:
  The drawing wand.
encoding:
  character string specifying text encoding

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (encoding :string))

(defcfun ("MagickDrawGetTextUnderColor" %MagickDrawGetTextUnderColor)
    :void
  "Returns the color of a background rectangle to place under text annotations.

The format of the DrawGetTextUnderColor method is:
  void DrawGetTextUnderColor\( const DrawingWand *drawing_wand, PixelWand *under_color );
drawing_wand:
  The drawing wand.
under_color:
  Return the under color.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (under_color %PixelWand))

(defcfun ("MagickDrawSetTextUnderColor" %MagickDrawSetTextUnderColor)
    :void
  "Specifies the color of a background rectangle to place under text annotations.

The format of the DrawSetTextUnderColor method is:
  void DrawSetTextUnderColor\( DrawingWand *drawing_wand, const PixelWand *under_wand );
drawing_wand:
  The drawing wand.
under_wand.:
  text under wand.

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (under_wand %PixelWand))

(defcfun ("MagickDrawTranslate" %MagickDrawTranslate)
    :void
  "Applies a translation to the current coordinate system which moves the coordinate system origin to the specified coordinate.

The format of the DrawTranslate method is:
  void DrawTranslate\( DrawingWand *drawing_wand, const double x, const double y );
drawing_wand:
  The drawing wand.
x:
  new x ordinate for coordinate system origin
y:
  new y ordinate for coordinate system origin

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x %double)
  (y %double))

(defcfun ("MagickDrawSetViewbox" %MagickDrawSetViewbox)
    :void
  "Sets the overall canvas size to be recorded with the drawing vector data. Usually this will be specified using the same size as the canvas image. When the vector data is saved to SVG or MVG formats, the viewbox is use to specify the size of the canvas image that a viewer will render the vector data on.

The format of the DrawSetViewbox method is:
  void DrawSetViewbox\( DrawingWand *drawing_wand, unsigned long x1, unsigned long y1,
                     unsigned long x2, unsigned long y2 );
drawing_wand:
  The drawing wand.
x1:
  left x ordinate
y1:
  top y ordinate
x2:
  right x ordinate
y2:
  bottom y ordinate

Since GraphicsMagick v1.1.0"
  (drawing_wand %DrawingWand)
  (x1 :unsigned-long)
  (y1 :unsigned-long)
  (x2 :unsigned-long)
  (y2 :unsigned-long))

(defcfun ("MagickNewDrawingWand" %MagickNewDrawingWand)
    %DrawingWand
  "Returns a drawing wand required for all other methods in the API.

The format of the NewDrawingWand method is:
  DrawingWand *NewDrawingWand\( void );

Since GraphicsMagick v1.1.0")
