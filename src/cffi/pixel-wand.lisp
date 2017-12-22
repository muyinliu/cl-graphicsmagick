(in-package :gm)

(defcfun ("ClonePixelWand" %ClonePixelWand)
    %PixelWand
  "Creates an exact copy of a PixelWand. PixelWand may not be a null pointer.

The format of the ClonePixelWand method is:
  PixelWand *ClonePixelWand\( const PixelWand *wand );

A description of each parameter follows:
wand:
  The pixel wand to clone.

Since GraphicsMagick v1.3.8"
  (wand %PixelWand))

(defcfun ("ClonePixelWands" %ClonePixelWands)
    :pointer
  "ClonePixelWands creates a deep-copy an array of PixelWands.

The format of the ClonePixelWands method is:
  PixelWand ** ClonePixelWands\( const PixelWand ** wands, const unsigned long number_wands );
wands:
  The pixel wands to clone.
number_wands:
  The number of wands in the array

Since GraphicsMagick v1.3.8"
  (wands :pointer)
  (number_wands :unsigned-long))

(defcfun ("DestroyPixelWand" %DestroyPixelWand)
    :void
  "Deallocates resources associated with a PixelWand.

The format of the DestroyPixelWand method is:
  void DestroyPixelWand\( PixelWand *wand );

A description of each parameter follows:
wand:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand))

(defcfun ("NewPixelWand" %NewPixelWand)
    %PixelWand
  "Returns a new pixel wand.

The format of the NewPixelWand method is:
  PixelWand NewPixelWand\( void );

Since GraphicsMagick v1.1.0")

(defcfun ("NewPixelWands" %NewPixelWands)
    %PixelWand
  "Returns an array of pixel wands.

The format of the NewPixelWand method is:
  PixelWand NewPixelWands\( const unsigned long number_wands );

A description of each parameter follows:
number_wands:
  The number of wands.

Since GraphicsMagick v1.1.0"
  (number_wands :unsigned-long))

(defcfun ("PixelGetException" %PixelGetException)
    :unsigned-int
  "Returns the severity, reason, and description of any error that occurs when using the pixel wand methods.

The format of the PixelGetException method is:
  unsigned int PixelGetException\( PixelWand *wand, char ** description );

A description of each parameter follows:
wand:
  The pixel wand.
description:
  A description of the error.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (description :pointer))

(defcfun ("PixelGetBlack" %PixelGetBlack)
    :double
  "Returns the normalized black color of the pixel wand.

The format of the PixelGetBlack method is:
  double PixelGetBlack\( const PixelWand *wand );

A description of each parameter follows:
wand:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand))

(defcfun ("PixelGetBlackQuantum" %PixelGetBlackQuantum)
    %Quantum
  "Returns the black color of the pixel wand. The color is in the range of [0..MaxRGB]

The format of the PixelGetBlackQuantum method is:
  Quantum PixelGetBlackQuantum\( const PixelWand *wand );

A description of each parameter follows:
wand:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand))

(defcfun ("PixelGetBlue" %PixelGetBlue)
    :double
  "PixelGetBlue\(const) returns the normalized blue color of the pixel wand.

The format of the PixelGetBlue method is:
  double PixelGetBlue\( const PixelWand *wand );

A description of each parameter follows:
wand:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand))

(defcfun ("PixelGetBlueQuantum" %PixelGetBlueQuantum)
    %Quantum
  "PixelGetBlueQuantum\(const ) returns the blue color of the pixel wand. The color is in the range of [0..MaxRGB]

The format of the PixelGetBlueQuantum method is:
  Quantum PixelGetBlueQuantum\( const PixelWand *wand );

A description of each parameter follows:
wand:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand))

(defcfun ("PixelGetColorAsString" %PixelGetColorAsString)
    :string
  "Gets the color of the pixel wand.

The format of the PixelGetColorAsString method is:
  char *PixelGetColorAsString\( PixelWand *wand );

A description of each parameter follows:
wand:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand))

(defcfun ("PixelGetColorCount" %PixelGetColorCount)
    :unsigned-long
  "Returns the color count associated with this color.

The format of the PixelGetColorCount method is:
  unsigned long PixelGetColorCount\( const PixelWand *wand );

A description of each parameter follows:
wand:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand))

(defcfun ("PixelGetCyan" %PixelGetCyan)
    :double
  "Returns the normalized cyan color of the pixel wand.

The format of the PixelGetCyan method is:
  double PixelGetCyan\( const PixelWand *wand );

A description of each parameter follows:
wand:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand))

(defcfun ("PixelGetCyanQuantum" %PixelGetCyanQuantum)
    %Quantum
  "Returns the cyan color of the pixel wand. The color is in the range of [0..MaxRGB]

The format of the PixelGetCyanQuantum method is:
  Quantum PixelGetCyanQuantum\( const PixelWand *wand );

A description of each parameter follows:
wand:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand))

(defcfun ("PixelGetGreen" %PixelGetGreen)
    :double
  "PixelGetGreen\(const ) returns the normalized green color of the pixel wand.

The format of the PixelGetGreen method is:
  double PixelGetGreen\( const PixelWand *wand );

A description of each parameter follows:
wand:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand))

(defcfun ("PixelGetGreenQuantum" %PixelGetGreenQuantum)
    %Quantum
  "PixelGetGreenQuantum\(const ) returns the green color of the pixel wand. The color is in the range of [0..MaxRGB]

The format of the PixelGetGreenQuantum method is:
  Quantum PixelGetGreenQuantum\( const PixelWand *wand );

A description of each parameter follows:
wand:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand))

(defcfun ("PixelGetMagenta" %PixelGetMagenta)
    :double
  "Returns the normalized magenta color of the pixel wand.

The format of the PixelGetMagenta method is:
  double PixelGetMagenta\( const PixelWand *wand );

A description of each parameter follows:
wand:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand))

(defcfun ("PixelGetMagentaQuantum" %PixelGetMagentaQuantum)
    %Quantum
  "Returns the magenta color of the pixel wand. The color is in the range of [0..MaxRGB]

The format of the PixelGetMagentaQuantum method is:
  Quantum PixelGetMagentaQuantum\( const PixelWand *wand );

A description of each parameter follows:
wand:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand))

(defcfun ("PixelGetOpacity" %PixelGetOpacity)
    :double
  "PixelGetOpacity\(const ) returns the normalized opacity color of the pixel wand.

The format of the PixelGetOpacity method is:
  double PixelGetOpacity\( const PixelWand *wand );

A description of each parameter follows:
wand:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand))

(defcfun ("PixelGetOpacityQuantum" %PixelGetOpacityQuantum)
    %Quantum
  "PixelGetOpacityQuantum\(const ) returns the opacity color of the pixel wand. The color is in the range of [0..MaxRGB]

The format of the PixelGetOpacityQuantum method is:
  Quantum PixelGetOpacityQuantum\( const PixelWand *wand );

A description of each parameter follows:
wand:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand))

(defcfun ("PixelGetQuantumColor" %PixelGetQuantumColor)
    :void
  "Ggets the color of the pixel wand.

The format of the PixelGetQuantumColor method is:
  PixelGetQuantumColor(PixelWand *wand,PixelPacket *color)

A description of each parameter follows:
wand:
  The pixel wand.
color:
  Return the pixel wand color here.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (color %PixelPacket))

(defcfun ("PixelGetRed" %PixelGetRed)
    :double
  "PixelGetRed\(const ) returns the normalized red color of the pixel wand.

The format of the PixelGetRed method is:
  double PixelGetRed\( const PixelWand *wand );

A description of each parameter follows:
wand:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand))

(defcfun ("PixelGetRedQuantum" %PixelGetRedQuantum)
    %Quantum
  "PixelGetRedQuantum\(const ) returns the red color of the pixel wand. The color is in the range of [0..MaxRGB]

The format of the PixelGetRedQuantum method is:
  Quantum PixelGetRedQuantum\( const PixelWand *wand );

A description of each parameter follows:
wand:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand))

(defcfun ("PixelGetYellow" %PixelGetYellow)
    :double
  "Returns the normalized yellow color of the pixel wand.

The format of the PixelGetYellow method is:
  double PixelGetYellow\( const PixelWand *wand );

A description of each parameter follows:
wand:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand))

(defcfun ("PixelGetYellowQuantum" %PixelGetYellowQuantum)
    %Quantum
  "Returns the yellow color of the pixel wand. The color is in the range of [0..MaxRGB]

The format of the PixelGetYellowQuantum method is:
  Quantum PixelGetYellowQuantum\( const PixelWand *wand );

A description of each parameter follows:
wand:
  The pixel wand.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand))

(defcfun ("PixelSetBlack" %PixelSetBlack)
    %pixel-status
  "Sets the normalized black color of the pixel wand.

The format of the PixelSetBlack method is:
  unsigned int PixelSetBlack\( PixelWand *wand, const double black );

A description of each parameter follows:
wand:
  The pixel wand.
black:
  The black color.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (black %double))

(defcfun ("PixelSetBlackQuantum" %PixelSetBlackQuantum)
    %pixel-status
  "Sets the black color of the pixel wand. The color must be in the range of [0..MaxRGB]

The format of the PixelSetBlackQuantum method is:
  unsigned int PixelSetBlackQuantum\( PixelWand *wand, const Quantum black );
wand:
  The pixel wand.
black:
  The black color.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (black %Quantum))

(defcfun ("PixelSetBlue" %PixelSetBlue)
    %pixel-status
  "Sets the normalized blue color of the pixel wand.

The format of the PixelSetBlue method is:
  unsigned int PixelSetBlue\( PixelWand *wand, const double blue );

A description of each parameter follows:
wand:
  The pixel wand.
blue:
  The blue color.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (blue %double))

(defcfun ("PixelSetBlueQuantum" %PixelSetBlueQuantum)
    %pixel-status
  "Sets the blue color of the pixel wand. The color must be in the range of [0..MaxRGB]

The format of the PixelSetBlueQuantum method is:
  unsigned int PixelSetBlueQuantum\( PixelWand *wand, const Quantum blue );

A description of each parameter follows:
wand:
  The pixel wand.
blue:
  The blue color.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (blue %Quantum))

(defcfun ("PixelSetColor" %PixelSetColor)
    %pixel-status
  "Sets the color of the pixel wand with a string \(e.g. \"blue\", \"#0000ff\", \"rgb\(0,0,255)\", etc.).

The format of the PixelSetColor method is:
  unsigned int PixelSetColor\( PixelWand *wand, const char *color );

A description of each parameter follows:
wand:
  The pixel wand.
color:
  The pixel wand color.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (color :string))

(defcfun ("PixelSetColorCount" %PixelSetColorCount)
    %pixel-status
  "Sets the color count of the pixel wand.

The format of the PixelSetColorCount method is:
  unsigned int PixelSetColorCount\( PixelWand *wand, const unsigned long count );
wand:
  The pixel wand.
count:
  The number of this particular color.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (count :unsigned-long))

(defcfun ("PixelSetCyan" %PixelSetCyan)
    %pixel-status
  "Sets the normalized cyan color of the pixel wand.

The format of the PixelSetCyan method is:
  unsigned int PixelSetCyan\( PixelWand *wand, const double cyan );

A description of each parameter follows:
wand:
  The pixel wand.
cyan:
  The cyan color.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (cyan %double))

(defcfun ("PixelSetCyanQuantum" %PixelSetCyanQuantum)
    %pixel-status
  "Sets the cyan color of the pixel wand. The color must be in the range of [0..MaxRGB]

The format of the PixelSetCyanQuantum method is:
  unsigned int PixelSetCyanQuantum\( PixelWand *wand, const Quantum cyan );

A description of each parameter follows:
wand:
  The pixel wand.
cyan:
  The cyan color.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (cyan %Quantum))

(defcfun ("PixelSetGreen" %PixelSetGreen)
    %pixel-status
  "Sets the normalized green color of the pixel wand.

The format of the PixelSetGreen method is:
  unsigned int PixelSetGreen\( PixelWand *wand, const double green );

A description of each parameter follows:
wand:
  The pixel wand.
green:
  The green color.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (green %double))

(defcfun ("PixelSetGreenQuantum" %PixelSetGreenQuantum)
    %pixel-status
  "Sets the green color of the pixel wand. The color must be in the range of [0..MaxRGB]

The format of the PixelSetGreenQuantum method is:
  unsigned int PixelSetGreenQuantum\( PixelWand *wand, const Quantum green );

A description of each parameter follows:
wand:
  The pixel wand.
green:
  The green color.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (green %Quantum))

(defcfun ("PixelSetMagenta" %PixelSetMagenta)
    %pixel-status
  "Sets the normalized magenta color of the pixel wand.

The format of the PixelSetMagenta method is:
  unsigned int PixelSetMagenta\( PixelWand *wand, const double magenta );

A description of each parameter follows:
wand:
  The pixel wand.
magenta:
  The magenta color.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (magenta %double))

(defcfun ("PixelSetMagentaQuantum" %PixelSetMagentaQuantum)
    %pixel-status
  "Sets the magenta color of the pixel wand. The color must be in the range of [0..MaxRGB]

The format of the PixelSetMagentaQuantum method is:
  unsigned int PixelSetMagentaQuantum\( PixelWand *wand, const Quantum magenta );
wand:
  The pixel wand.
magenta:
  The magenta color.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (magenta %Quantum))

(defcfun ("PixelSetOpacity" %PixelSetOpacity)
    %pixel-status
  "Sets the normalized opacity color of the pixel wand.

The format of the PixelSetOpacity method is:
  unsigned int PixelSetOpacity\( PixelWand *wand, const double opacity );

A description of each parameter follows:
wand:
  The pixel wand.
opacity:
  The opacity value.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (opacity %double))

(defcfun ("PixelSetOpacityQuantum" %PixelSetOpacityQuantum)
    %pixel-status
  "Sets the opacity color of the pixel wand. The color must be in the range of [0..MaxRGB]

The format of the PixelSetOpacityQuantum method is:
  unsigned int PixelSetOpacityQuantum\( PixelWand *wand, const Quantum opacity );
wand:
  The pixel wand.
opacity:
  The opacity value.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (opacity %Quantum))

(defcfun ("PixelSetQuantumColor" %PixelSetQuantumColor)
    :void
  "Sets the color of the pixel wand.

The format of the PixelSetQuantumColor method is:
  PixelSetQuantumColor\( PixelWand *wand, PixelPacket *color );

A description of each parameter follows:
wand:
  The pixel wand.
color:
  The pixel wand color \(expressed as a PixelPacket).

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (color %PixelPacket))

(defcfun ("PixelSetRed" %PixelSetRed)
    %pixel-status
  "Sets the normalized red color of the pixel wand.

The format of the PixelSetRed method is:
  unsigned int PixelSetRed\( PixelWand *wand, const double red );

A description of each parameter follows:
wand:
  The pixel wand.
red:
  The red color.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (red %double))

(defcfun ("PixelSetRedQuantum" %PixelSetRedQuantum)
    %pixel-status
  "Sets the red color of the pixel wand. The color must be in the range of [0..MaxRGB]

The format of the PixelSetRedQuantum method is:
  unsigned int PixelSetRedQuantum\( PixelWand *wand, const Quantum red );

A description of each parameter follows:
wand:
  The pixel wand.
red:
  The red color.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (red %Quantum))

(defcfun ("PixelSetYellow" %PixelSetYellow)
    %pixel-status
  "Sets the normalized yellow color of the pixel wand.

The format of the PixelSetYellow method is:
  unsigned int PixelSetYellow\( PixelWand *wand, const double yellow );

A description of each parameter follows:
wand:
  The pixel wand.
yellow:
  The yellow color.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (yellow %double))

(defcfun ("PixelSetYellowQuantum" %PixelSetYellowQuantum)
    %pixel-status
  "Sets the yellow color of the pixel wand. The color must be in the range of [0..MaxRGB]

The format of the PixelSetYellowQuantum method is:
  unsigned int PixelSetYellowQuantum\( PixelWand *wand, const Quantum yellow );

A description of each parameter follows:
wand:
  The pixel wand.
yellow:
  The yellow color.

Since GraphicsMagick v1.1.0"
  (wand %PixelWand)
  (yellow %Quantum))
