#' @title NGCircos
#'
#' @description A R packages based on Next Generation Circos
#'
#' @import htmlwidgets
#' @import RColorBrewer
#' @import plyr
#' @import jsonlite
#' @import grDevices
#'
#' @param tracklist A list of module to display.
#' @param genome A list of chromosome lengths to be used as reference for the visualization or 'hg19' to use
#'  the chromosomes 1 to 22 and the sexual chromosomes according to the hg19 reference.
#' @param genome2 Second genome when compare module is applied
#' @param genomeFillColor The color to display in each chromosome. Can be a RColorBrewer palette name used to
#'  generate one color per chromosome, or a character object or vector of character objects stating RGB values in hexadecimal
#'  format or base R colors. If the vector is shorter than the reference genome, values will be repeated.
#' @param chrPad Distance between chromosomes.
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param svgClassName The svg class name
#' @param innerRadius Default 216, Inner radius of chromosome
#' @param outerRadius Default 240, Outer radius of chromosome
#' @param displayGenomeBorder,genomeBorderColor,genomeBorderSize Should the reference genome have borders?
#'  If yes specify the color, in RGB hexadecimal format, and the thickness.
#' @param genomeTicksDisplay,genomeTicksLen,genomeTicksColor,genomeTicksTextSize,genomeTicksTextColor,genomeTicksScale,genomeTicksRealLength,genomeTicksOffset
#'  Should the refence genome have ticks, of which length, color (in hexadecimal RGB format), with labels in which font
#' size and color, and spaced by how many bases? whether show the real length of genome? The offset from real length
#' @param genomeLabelDisplay,genomeLabelTextSize,genomeLabelTextColor,genomeLabelDx,genomeLabelDy
#'  Should the reference genome have labels on each chromosome, in which font size and color?
#' @param compareEvent  Default False, open/not COMPARE module
#' @param compareEventGroupGapRate Default 0.1, control the two-side gap rate on each group of genome
#' @param compareEventGroupDistance Default 0, distance between two groups of genome
#' @param zoom Is zooming and moving in the visualization allowed?
#'
#' @param TEXTModuleDragEvent Are text annotations draggable?
#'
#'
#' @param SNPxlink Default False, add/not xlink for SNP module
#' @param SNPMouseEvent Default True, open/not open mouse event of SNP module
#' @param SNPMouseCombinationEvent Default False, open/not COMBINATION module for SNP module
#' @param SNPMouseCombinationImageDisplay Defalut False, open/not image display in COMBINATION module for SNP module
#' @param SNPMouseCombinationImageTitle Title of the image
#' @param SNPMouseCombinationImageTitleSize,SNPMouseCombinationImageTitleWeight,SNPMouseCombinationImageTitleColor Size, weight and color of the title
#' @param SNPMouseCombinationImagePositionX,SNPMouseCombinationImagePositionY Coordinates for image
#' @param SNPMouseCombinationImageHeight,SNPMouseCombinationImageWidth Height and width of image
#' @param SNPMouseCombinationGraphDisplay Defalut False, open/not graph display in COMBINATION module for SNP module
#' @param SNPMouseCombinationGraphTitle Title of the graph
#' @param SNPMouseCombinationGraphTitleSize,SNPMouseCombinationGraphTitleWeight,SNPMouseCombinationGraphTitleColor Size, weight and color of the title
#' @param SNPMouseCombinationGraphType Type of graph
#' @param SNPMouseCombinationGraphPositionX,SNPMouseCombinationGraphPositionY Coordinates for graph
#' @param SNPMouseCombinationGraphHeight,SNPMouseCombinationGraphWidth Height and width for graph
#' @param SNPMouseCombinationGraphHistogramBarColor Bar color of histogram graph
#' @param SNPMouseCombinationGraphHistogramPadding Padding between bar of histogram graph
#' @param SNPMouseCombinationGraphHistogramPositionCorrectX Correction distance of X axis in histogram
#' @param SNPMouseCombinationGraphPieAutoColor Whether use auto color for pie graph or not
#' @param SNPMouseCombinationGraphPieColor Color for pie graph if auto color is false
#' @param SNPMouseCombinationGraphPieSize Size of pie graph
#' @param SNPMouseCombinationGraphPieStroke Whether each pie has a stroke or not
#' @param SNPMouseCombinationGraphPieStrokeColor,SNPMouseCombinationGraphPieStrokeWidth The stroke color and width for pie graph
#' @param SNPMouseCombinationGraphPieOpacity Opacity for pie graph
#' @param SNPMouseCombinationGraphLineType,SNPMouseCombinationGraphLineColor,SNPMouseCombinationGraphLineWidth Line type, color and width for line graph
#' @param SNPMouseCombinationGraphLinePoint Whether display the broken point in line graph
#' @param SNPMouseCombinationGraphLinePointSize Size of broken point
#' @param SNPMouseCombinationGraphLinePointAutoColor Whether display the broken point in auto color
#' @param SNPMouseCombinationGraphLinePointColor Color for broken point if auto color is false
#' @param SNPMouseCombinationGraphLinePointStroke Whether display the broken point stroke
#' @param SNPMouseCombinationGraphLinePointStrokeColor,SNPMouseCombinationGraphLinePointStrokeWidth The stroke color and width for broken point
#' @param SNPMouseCombinationGraphLinePointOpacity Opacity for broken line
#' @param SNPMouseCombinationGraphLinePositionCorrectX Correction distance of X axis for line
#' @param SNPMouseCombinationTextDisplay Defalut False, open/not text display in COMBINATION module for SNP module
#' @param SNPMouseCombinationTextColor,SNPMouseCombinationTextSize,SNPMouseCombinationTextWeight The color, size and weight for text
#' @param SNPMouseCombinationTextPositionCorrectX,SNPMouseCombinationTextPositionCorrectY The coordinates for text
#' @param SNPMouseClickDisplay Default False, show/not the tooltip when mouse click on a SNP point.
#' @param SNPMouseClickColor Color after clicking the element
#' @param SNPMouseClickCircleSize Circle size after clicking the element
#' @param SNPMouseClickCircleOpacity Opacity after clicking the element
#' @param SNPMouseClickCircleStrokeColor Stroke color after clicking the element
#' @param SNPMouseClickCircleStrokeWidth Stroke width after clicking the element
#' @param SNPMouseClickTextFromData First,second,third,fourth column data click to show
#' @param SNPMouseClickTextOpacity Text opacity after clicking the element
#' @param SNPMouseClickTextColor Text color after clicking the element
#' @param SNPMouseClickTextSize Text size after clicking the element
#' @param SNPMouseClickTextPostionX,SNPMouseClickTextPostionY Text coordinate after clicking the element
#' @param SNPMouseClickTextDrag Whether text is draggable for element
#' @param SNPMouseUpDisplay Default False, show/not the tooltip when mouse click up a SNP point.
#' @param SNPMouseUpColor Color after mouse moving up the element
#' @param SNPMouseUpCircleSize Circle size after mouse moving up the element
#' @param SNPMouseUpCircleOpacity Circle opacity after mouse moving up the element
#' @param SNPMouseUpCircleStrokeColor Circle stroke color after mouse moving up the element
#' @param SNPMouseUpCircleStrokeWidth Circle stroke width after mouse moving up the element
#' @param SNPMouseDownDisplay Default False, show/not the tooltip when mouse click down a SNP point.
#' @param SNPMouseDownColor Color after mouse moving down the element
#' @param SNPMouseDownCircleSize Circle size after mouse moving down the element
#' @param SNPMouseDownCircleOpacity Circle opacity after mouse moving down the element
#' @param SNPMouseDownCircleStrokeColor Circle stroke color after mouse moving down the element
#' @param SNPMouseDownCircleStrokeWidth Circle stroke width after mouse moving down the element
#' @param SNPMouseEnterDisplay Default False, show/not the tooltip when mouse mover over a SNP point.
#' @param SNPMouseEnterColor Color after mouse entering enter the element
#' @param SNPMouseEnterCircleSize Circle size after mouse entering the element
#' @param SNPMouseEnterCircleOpacity Circle opacity after mouse entering the element
#' @param SNPMouseEnterCircleStrokeColor Circle stroke color after mouse entering the element
#' @param SNPMouseEnterCircleStrokeWidth Circle stroke width after mouse entering the element
#' @param SNPMouseLeaveDisplay Default False, show/not the tooltip when mouse mover leave a SNP point.
#' @param SNPMouseLeaveColor Color after mouse leaving the element
#' @param SNPMouseLeaveCircleSize Circle size after mouse leaving the element
#' @param SNPMouseLeaveCircleOpacity Circle opacity after mouse leaving the element
#' @param SNPMouseLeaveCircleStrokeColor Circle stroke color after mouse leaving the element
#' @param SNPMouseLeaveCircleStrokeWidth Circle stroke width after mouse leaving the element
#' @param SNPMouseMoveDisplay Default False, show/not the tooltip when mouse move into a SNP point.
#' @param SNPMouseMoveColor Color after mouse moving in the element
#' @param SNPMouseMoveCircleSize Circle size after mouse moving in the element
#' @param SNPMouseMoveCircleOpacity Circle opacity after mouse moving in the element
#' @param SNPMouseMoveCircleStrokeColor Circle stroke color after mouse moving in the element
#' @param SNPMouseMoveCircleStrokeWidth Circle stroke width after mouse moving in the element
#' @param SNPMouseOverDisplay Default False, show/not the tooltip when mouse hover on a SNP point.
#' @param SNPMouseOverColor Color after mouse moving over the element
#' @param SNPMouseOverCircleSize Circle size after mouse moving over the element
#' @param SNPMouseOverCircleOpacity Circle opacity after mouse moving over the element
#' @param SNPMouseOverCircleStrokeColor Circle stroke color after mouse moving over the element
#' @param SNPMouseOverCircleStrokeWidth Circle stroke width after mouse moving over the element
#' @param SNPMouseOverTooltipsSetting Default "chr : "
#' @param SNPMouseOverTooltipsHtml Default " "
#' @param SNPMouseOverTooltipsPosition Position for tooltips when mouse moving over
#' @param SNPMouseOverTooltipsBackgroundColor Background color for tooltips when mouse moving over
#' @param SNPMouseOverTooltipsBorderStyle Border style for tooltips when mouse moving over
#' @param SNPMouseOverTooltipsBorderWidth Border width for tooltips when mouse moving over
#' @param SNPMouseOverTooltipsPadding Padding for tooltips when mouse moving over
#' @param SNPMouseOverTooltipsBorderRadius Border radius for tooltips when mouse moving over
#' @param SNPMouseOverTooltipsOpacity Opacity for tooltips when mouse moving over
#' @param SNPMouseOutDisplay Defalut False, hide/not tooltip when mouse is not hovering a SNP point anymore.
#' @param SNPMouseOutAnimationTime Animation time when mouse moving over the element
#' @param SNPMouseOutColor Color when mouse moving over the element
#' @param SNPMouseOutCircleSize Circle size when mouse moving over the element
#' @param SNPMouseOutCircleOpacity Opacity when mouse moving over the element
#' @param SNPMouseOutCircleStrokeColor Stroke color when mouse moving over the element
#' @param SNPMouseOutCircleStrokeWidth Stroke width when mouse moving over the element
#'
#' @param LINKxlink Default False, add/not xlink for LINK module
#' @param LINKMouseEvent Default True, open/not open mouse event of LINK module
#' @param LINKMouseClickDisplay Default False, show/not the tooltip when mouse click on a LINK point.
#' @param LINKMouseClickOpacity Opacity when mouse clicking
#' @param LINKMouseClickStrokeColor Stroke color when mouse clicking
#' @param LINKMouseClickStrokeWidth Stroke width when mouse clicking
#' @param LINKMouseUpDisplay Default False, show/not the tooltip when mouse click up a LINK point.
#' @param LINKMouseUpOpacity Opacity when mouse moving up the element
#' @param LINKMouseUpStrokeColor Stroke color when mouse moving up the element
#' @param LINKMouseUpStrokeWidth Stroke width when mouse moving up the element
#' @param LINKMouseDownDisplay Default False, show/not the tooltip when mouse click down a LINK point.
#' @param LINKMouseDownOpacity Opacity when mouse moving down the element
#' @param LINKMouseDownStrokeColor Stroke color when mouse moving down the element
#' @param LINKMouseDownStrokeWidth Stroke width when mouse moving down the element
#' @param LINKMouseEnterDisplay Default False, show/not the tooltip when mouse mover over a LINK point.
#' @param LINKMouseEnterOpacity Opacity when mouse entering the element
#' @param LINKMouseEnterStrokeColor Stroke color when mouse entering the element
#' @param LINKMouseEnterStrokeWidth Stroke width when mouse entering the element
#' @param LINKMouseLeaveDisplay Default False, show/not the tooltip when mouse mover leave a LINK point.
#' @param LINKMouseLeaveOpacity Opacity when mouse leaving the element
#' @param LINKMouseLeaveStrokeColor Stroke color when mouse leaving the element
#' @param LINKMouseLeaveStrokeWidth Stroke width when mouse leaving the element
#' @param LINKMouseMoveDisplay Default False, show/not the tooltip when mouse move into a LINK point.
#' @param LINKMouseMoveOpacity Opacity when mouse moving in the element
#' @param LINKMouseMoveStrokeColor Stroke color when mouse moving in the element
#' @param LINKMouseMoveStrokeWidth Stroke width when mouse moving in the element
#' @param LINKMouseOverDisplay Default False, show/not the tooltip when mouse hover on a LINK point.
#' @param LINKMouseOverOpacity Opacity when mouse moving over the element
#' @param LINKMouseOverStrokeColor Stroke color when mouse moving over the element
#' @param LINKMouseOverStrokeWidth Stroke width when mouse moving over the element
#' @param LINKMouseOverTooltipsSetting Default "style1"
#' @param LINKMouseOverTooltipsHtml Default " "
#' @param LINKMouseOverTooltipsPosition Default "absolute"
#' @param LINKMouseOverTooltipsBackgroundColor Default "white"
#' @param LINKMouseOverTooltipsBorderStyle Default "solid"
#' @param LINKMouseOverTooltipsBorderWidth Default 0
#' @param LINKMouseOverTooltipsPadding Default "3px"
#' @param LINKMouseOverTooltipsBorderRadius Default "3px"
#' @param LINKMouseOverTooltipsOpacity Default 0.8
#' @param LINKMouseOutDisplay Defalut False, hide/not tooltip when mouse is not hovering a LINK point anymore.
#' @param LINKMouseOutAnimationTime Animation time when mouse moving out the element
#' @param LINKMouseOutOpacity Opacity when mouse moving out the element
#' @param LINKMouseOutStrokeColor Stroke color when mouse moving out the element
#' @param LINKMouseOutStrokeWidth Stroke width when mouse moving out the element
#' @param LINKLabelDragEvent Defalut False, draggable for the label of LINK module
#'
#' @param CHORDMouseEvent Default True, open/not open mouse event of CHORD module
#' @param CHORDMouseFillColorExcluded A type of color in character, chord in this color will be hided
#' @param CHORDMouseClickDisplay Default False, show/not the tooltip when mouse click on a CHORD point.
#' @param CHORDMouseClickOpacity Opacity when mouse clicking
#' @param CHORDMouseClickStrokeColor Stroke color when mouse clicking
#' @param CHORDMouseClickStrokeWidth Stroke width when mouse clicking
#' @param CHORDMouseDownDisplay Default False, show/not the tooltip when mouse click down a CHORD point.
#' @param CHORDMouseDownOpacity Opacity when mouse moving down the element
#' @param CHORDMouseDownStrokeColor Stroke color when mouse moving down the element
#' @param CHORDMouseDownStrokeWidth Stroke width when mouse moving down the element
#' @param CHORDMouseEnterDisplay Default False, show/not the tooltip when mouse mover over a CHORD point.
#' @param CHORDMouseEnterOpacity Opacity when mouse entering the element
#' @param CHORDMouseEnterStrokeColor Stroke color when mouse entering the element
#' @param CHORDMouseEnterStrokeWidth Stroke width when mouse entering the element
#' @param CHORDMouseLeaveDisplay Default False, show/not the tooltip when mouse mover leave a CHORD point.
#' @param CHORDMouseLeaveOpacity Opacity when mouse leaving the element
#' @param CHORDMouseLeaveStrokeColor Stroke color when mouse leaving the element
#' @param CHORDMouseLeaveStrokeWidth Stroke width when mouse leaving the element
#' @param CHORDMouseMoveDisplay Default False, show/not the tooltip when mouse move into a CHORD point.
#' @param CHORDMouseMoveOpacity Opacity when mouse moving in the element
#' @param CHORDMouseMoveStrokeColor Stroke color when mouse moving in the element
#' @param CHORDMouseMoveStrokeWidth Stroke width when mouse moving in the element
#' @param CHORDMouseOutDisplay Defalut False, hide/not tooltip when mouse is not hovering a CHORD point anymore.
#' @param CHORDMouseOutAnimationTime Animation time when mouse moving out the element
#' @param CHORDMouseOutOpacity Opacity when mouse moving out the element
#' @param CHORDMouseOutStrokeColor Stroke color when mouse moving out the element
#' @param CHORDMouseOutStrokeWidth Stroke width when mouse moving out the element
#' @param CHORDMouseUpDisplay Default False, show/not the tooltip when mouse click up a CHORD point.
#' @param CHORDMouseUpOpacity Opacity when mouse moving up the element
#' @param CHORDMouseUpStrokeColor Stroke color when mouse moving up the element
#' @param CHORDMouseUpStrokeWidth Stroke width when mouse moving up the element
#' @param CHORDMouseOverDisplay Default False, show/not the tooltip when mouse hover on a CHORD point.
#' @param CHORDMouseOverOpacity Opacity when mouse moving over the element
#' @param CHORDMouseOverStrokeColor Stroke color when mouse moving over the element
#' @param CHORDMouseOverStrokeWidth Stroke width when mouse moving over the element
#'
#' @param HISTOGRAMxlink Default False, add/not xlink for HISTOGRAM module
#' @param HISTOGRAMMouseEvent Default True, open/not open mouse event of HISTOGRAM module
#' @param HISTOGRAMMouseClickDisplay Default False, show/not the tooltip when mouse click on a HISTOGRAM point.
#' @param HISTOGRAMMouseClickColor Color when mouse clicking
#' @param HISTOGRAMMouseClickOpacity Opacity when mouse clicking
#' @param HISTOGRAMMouseClickStrokeColor Stroke color when mouse clicking
#' @param HISTOGRAMMouseClickStrokeWidth Stroke width when mouse clicking
#' @param HISTOGRAMMouseUpDisplay Default False, show/not the tooltip when mouse click up a HISTOGRAM point.
#' @param HISTOGRAMMouseUpColor Color when mouse moving up the element
#' @param HISTOGRAMMouseUpOpacity Opacity when mouse moving up the element
#' @param HISTOGRAMMouseUpStrokeColor Stroke color when mouse moving up the element
#' @param HISTOGRAMMouseUpStrokeWidth Stroke width when mouse moving up the element
#' @param HISTOGRAMMouseDownDisplay Default False, show/not the tooltip when mouse click down a HISTOGRAM point.
#' @param HISTOGRAMMouseDownColor Color when mouse moving down the element
#' @param HISTOGRAMMouseDownOpacity Opacity when mouse moving up the element
#' @param HISTOGRAMMouseDownStrokeColor Stroke color when mouse moving up the element
#' @param HISTOGRAMMouseDownStrokeWidth Stroke width when mouse moving up the element
#' @param HISTOGRAMMouseEnterDisplay Default False, show/not the tooltip when mouse mover over a HISTOGRAM point.
#' @param HISTOGRAMMouseEnterColor Color when mouse entering the element
#' @param HISTOGRAMMouseEnterOpacity Opacity when mouse entering the element
#' @param HISTOGRAMMouseEnterStrokeColor Stroke color when mouse entering the element
#' @param HISTOGRAMMouseEnterStrokeWidth Stroke width when mouse entering the element
#' @param HISTOGRAMMouseLeaveDisplay Default False, show/not the tooltip when mouse mover leave a HISTOGRAM point.
#' @param HISTOGRAMMouseLeaveColor Color when mouse leaving the element
#' @param HISTOGRAMMouseLeaveOpacity Opacity when mouse leaving the element
#' @param HISTOGRAMMouseLeaveStrokeColor Stroke color when mouse leaving the element
#' @param HISTOGRAMMouseLeaveStrokeWidth Stroke width when mouse leaving the element
#' @param HISTOGRAMMouseMoveDisplay Default False, show/not the tooltip when mouse move into a HISTOGRAM point.
#' @param HISTOGRAMMouseMoveColor Color when mouse moving in the element
#' @param HISTOGRAMMouseMoveOpacity Opacity when mouse moving in the element
#' @param HISTOGRAMMouseMoveStrokeColor Stroke color when mouse moving in the element
#' @param HISTOGRAMMouseMoveStrokeWidth Stroke width when mouse moving in the element
#' @param HISTOGRAMMouseOverDisplay Default False, show/not the tooltip when mouse hover on a HISTOGRAM point.
#' @param HISTOGRAMMouseOverColor Color when mouse moving over the element
#' @param HISTOGRAMMouseOverOpacity Opacity when mouse moving over the element
#' @param HISTOGRAMMouseOverStrokeColor Stroke color when mouse moving over the element
#' @param HISTOGRAMMouseOverStrokeWidth Stroke width when mouse moving over the element
#' @param HISTOGRAMMouseOverTooltipsSetting Default "style1"
#' @param HISTOGRAMMouseOverTooltipsHtml Default " "
#' @param HISTOGRAMMouseOverTooltipsPosition Default "absolute"
#' @param HISTOGRAMMouseOverTooltipsBackgroundColor Default "white"
#' @param HISTOGRAMMouseOverTooltipsBorderStyle Default "solid"
#' @param HISTOGRAMMouseOverTooltipsBorderWidth Default 0
#' @param HISTOGRAMMouseOverTooltipsPadding Default "3px"
#' @param HISTOGRAMMouseOverTooltipsBorderRadius Default "3px"
#' @param HISTOGRAMMouseOverTooltipsOpacity Default 0.8
#' @param HISTOGRAMMouseOutDisplay Defalut False, hide/not tooltip when mouse is not hovering a HISTOGRAM point anymore.
#' @param HISTOGRAMMouseOutAnimationTime Animation time when mouse moving out the element
#' @param HISTOGRAMMouseOutColor Color when mouse moving out the element
#' @param HISTOGRAMMouseOutOpacity Opacity when mouse moving out the element
#' @param HISTOGRAMMouseOutStrokeColor Stroke color when mouse moving out the element
#' @param HISTOGRAMMouseOutStrokeWidth Stroke width when mouse moving out the element
#'
#' @param LINEMouseEvent Default True, open/not open mouse event of LINE module
#' @param LINEMouseClickDisplay Default False, show/not the tooltip when mouse click on a LINE point.
#' @param LINEMouseClickLineOpacity Line opacity when mouse clicking the element
#' @param LINEMouseClickLineStrokeColor Stroke color when mouse clicking the element
#' @param LINEMouseClickLineStrokeWidth Stroke width when mouse clicking the element
#' @param LINEMouseDownDisplay Default False, show/not the tooltip when mouse click down a LINE point.
#' @param LINEMouseDownLineOpacity Line opacity when mouse moving down the element
#' @param LINEMouseDownLineStrokeColor Stroke color when mouse moving down the element
#' @param LINEMouseDownLineStrokeWidth Stroke width when mouse moving down the element
#' @param LINEMouseEnterDisplay Default False, show/not the tooltip when mouse mover over a LINE point.
#' @param LINEMouseEnterLineOpacity Line opacity when mouse entering the element
#' @param LINEMouseEnterLineStrokeColor Stroke color when mouse entering the element
#' @param LINEMouseEnterLineStrokeWidth Stroke width when mouse entering the element
#' @param LINEMouseLeaveDisplay Default False, show/not the tooltip when mouse mover leave a LINE point.
#' @param LINEMouseLeaveLineOpacity Line opacity when mouse leaving the element
#' @param LINEMouseLeaveLineStrokeColor Stroke color when mouse leaving the element
#' @param LINEMouseLeaveLineStrokeWidth Stroke width when mouse leaving the element
#' @param LINEMouseMoveDisplay Default False, show/not the tooltip when mouse move into a LINE point.
#' @param LINEMouseMoveLineOpacity Line opacity when mouse moving in the element
#' @param LINEMouseMoveLineStrokeColor Stroke color when mouse moving in the element
#' @param LINEMouseMoveLineStrokeWidth Stroke width when mouse moving in the element
#' @param LINEMouseOutDisplay Defalut False, hide/not tooltip when mouse is not hovering a LINE point anymore.
#' @param LINEMouseOutAnimationTime Animation time when mouse moving out the element
#' @param LINEMouseOutLineOpacity Line opacity when mouse moving out the element
#' @param LINEMouseOutLineStrokeColor Stroke color when mouse moving out the element
#' @param LINEMouseOutLineStrokeWidth Stroke width when mouse moving out the element
#' @param LINEMouseUpDisplay Default False, show/not the tooltip when mouse click up a LINE point.
#' @param LINEMouseUpLineOpacity Line opacity when mouse moving up the element
#' @param LINEMouseUpLineStrokeColor Stroke color when mouse moving up the element
#' @param LINEMouseUpLineStrokeWidth Stroke width when mouse moving up the element
#' @param LINEMouseOverDisplay Default False, show/not the tooltip when mouse hover on a LINE point.
#' @param LINEMouseOverLineOpacity  Line opacity when mouse moving over the element
#' @param LINEMouseOverLineStrokeColor Stroke color when mouse moving over the element
#' @param LINEMouseOverLineStrokeWidth Stroke width when mouse moving over the element
#' @param LINEMouseOverTooltipsSetting Default "style1"
#' @param LINEMouseOverTooltipsHtml Default " "
#' @param LINEMouseOverTooltipsPosition Default "absolute"
#' @param LINEMouseOverTooltipsBackgroundColor Default "white"
#' @param LINEMouseOverTooltipsBorderStyle Default "solid"
#' @param LINEMouseOverTooltipsBorderWidth Default 0
#' @param LINEMouseOverTooltipsPadding Default "3px"
#' @param LINEMouseOverTooltipsBorderRadius Default "3px"
#' @param LINEMouseOverTooltipsOpacity Default 0.8
#'
#' @param WIGMouseEvent Default True, open/not open mouse event of WIG module
#' @param WIGMouseClickDisplay Default False, show/not the tooltip when mouse click on a WIG point.
#' @param WIGMouseClickLineOpacity Line opacity when mouse clicking the element
#' @param WIGMouseClickLineStrokeColor Stroke color when mouse clicking the element
#' @param WIGMouseClickLineStrokeWidth Stroke width when mouse clicking the element
#' @param WIGMouseClickFillColor Filling color when mouse clicking the element
#' @param WIGMouseDownDisplay Default False, show/not the tooltip when mouse click down a WIG point.
#' @param WIGMouseDownLineOpacity Line opacity when mouse moving down the element
#' @param WIGMouseDownLineStrokeColor Stroke color when mouse moving down the element
#' @param WIGMouseDownLineStrokeWidth Stroke width when mouse moving down the element
#' @param WIGMouseDownFillColor Filling color when mouse moving down the element
#' @param WIGMouseEnterDisplay Default False, show/not the tooltip when mouse mover over a WIG point.
#' @param WIGMouseEnterLineOpacity Line opacity when mouse entering the element
#' @param WIGMouseEnterLineStrokeColor Stroke color when mouse entering the element
#' @param WIGMouseEnterLineStrokeWidth Stroke width when mouse entering the element
#' @param WIGMouseEnterFillColor Filling color when mouse entering the element
#' @param WIGMouseLeaveDisplay Default False, show/not the tooltip when mouse mover leave a WIG point.
#' @param WIGMouseLeaveLineOpacity Line opacity when mouse leaving the element
#' @param WIGMouseLeaveLineStrokeColor Stroke color when mouse leaving the element
#' @param WIGMouseLeaveLineStrokeWidth Stroke width when mouse leaving the element
#' @param WIGMouseLeaveFillColor Filling color when mouse leaving the element
#' @param WIGMouseMoveDisplay Default False, show/not the tooltip when mouse move into a WIG point.
#' @param WIGMouseMoveLineOpacity Line opacity when mouse moving in the element
#' @param WIGMouseMoveLineStrokeColor Stroke color when mouse moving in the element
#' @param WIGMouseMoveLineStrokeWidth Stroke width when mouse moving in the element
#' @param WIGMouseMoveFillColor Filling color when mouse leaving the element
#' @param WIGMouseOutDisplay Defalut False, hide/not tooltip when mouse is not hovering a WIG point anymore.
#' @param WIGMouseOutAnimationTime Animation time when mouse moving out the element
#' @param WIGMouseOutLineOpacity Line opacity when mouse moving out the element
#' @param WIGMouseOutLineStrokeColor Stroke color when mouse moving out the element
#' @param WIGMouseOutLineStrokeWidth Stroke width when mouse moving out the element
#' @param WIGMouseOutFillColor Filling color when mouse moving out the element
#' @param WIGMouseUpDisplay Default False, show/not the tooltip when mouse click up a WIG point.
#' @param WIGMouseUpLineOpacity Line opacity when mouse moving up the element
#' @param WIGMouseUpLineStrokeColor Stroke color when mouse moving up the element
#' @param WIGMouseUpLineStrokeWidth Stroke width when mouse moving up the element
#' @param WIGMouseUpFillColor Filling color when mouse moving up the element
#' @param WIGMouseOverDisplay Default False, show/not the tooltip when mouse hover on a WIG point.
#' @param WIGMouseOverLineOpacity Line opacity when mouse moving over the element
#' @param WIGMouseOverLineStrokeColor Stroke color when mouse moving over the element
#' @param WIGMouseOverLineStrokeWidth Stroke width when mouse moving over the element
#' @param WIGMouseOverFillColor Filling color when mouse moving over the element
#' @param WIGMouseOverTooltipsSetting Default "style1"
#' @param WIGMouseOverTooltipsHtml Default " "
#' @param WIGMouseOverTooltipsPosition Default "absolute"
#' @param WIGMouseOverTooltipsBackgroundColor Default "white"
#' @param WIGMouseOverTooltipsBorderStyle Default "solid"
#' @param WIGMouseOverTooltipsBorderWidth Default 0
#' @param WIGMouseOverTooltipsPadding Default "3px"
#' @param WIGMouseOverTooltipsBorderRadius Default "3px"
#' @param WIGMouseOverTooltipsOpacity Default 0.8
#'
#' @param SCATTERxlink Default False, add/not xlink for SCATTER module
#' @param SCATTERMouseEvent Default True, open/not open mouse event of SCATTER module
#' @param SCATTERMouseClickDisplay Default False, show/not the tooltip when mouse click on a SCATTER point.
#' @param SCATTERMouseClickColor Color when mouse clicking  the element
#' @param SCATTERMouseClickCircleSize Circle size when mouse clicking the element
#' @param SCATTERMouseClickCircleOpacity Circle opacity when mouse clicking the element
#' @param SCATTERMouseClickCircleStrokeColor Circle stroke color when mouse clicking the element
#' @param SCATTERMouseClickCircleStrokeWidth Circle stroke width when mouse clicking the element
#' @param SCATTERMouseClickTextFromData Text column when mouse clicking the element
#' @param SCATTERMouseClickTextOpacity Text opacity when mouse clicking the element
#' @param SCATTERMouseClickTextColor Text color when mouse clicking the element
#' @param SCATTERMouseClickTextSize Text size when mouse clicking the element
#' @param SCATTERMouseClickTextPostionX,SCATTERMouseClickTextPostionY Text coordinates when mouse clicking the element
#' @param SCATTERMouseClickTextDrag Whether text is draggable when clicing element
#' @param SCATTERMouseUpDisplay Default False, show/not the tooltip when mouse click up a SCATTER point.
#' @param SCATTERMouseUpColor Color when mouse moving up the element
#' @param SCATTERMouseUpCircleSize Circle size when mouse moving up the element
#' @param SCATTERMouseUpCircleOpacity Circle opacity when mouse moving up the element
#' @param SCATTERMouseUpCircleStrokeColor Circle stroke color when mouse moving up the element
#' @param SCATTERMouseUpCircleStrokeWidth Circle stroke width when mouse moving up the element
#' @param SCATTERMouseDownDisplay Default False, show/not the tooltip when mouse click down a SCATTER point.
#' @param SCATTERMouseDownColor Color when mouse moving down the element
#' @param SCATTERMouseDownCircleSize Circle size when mouse moving down the element
#' @param SCATTERMouseDownCircleOpacity Circle opacity when mouse moving down the element
#' @param SCATTERMouseDownCircleStrokeColor Circle stroke color when mouse moving down the element
#' @param SCATTERMouseDownCircleStrokeWidth Circle stroke width when mouse moving down the element
#' @param SCATTERMouseEnterDisplay Default False, show/not the tooltip when mouse mover over a SCATTER point.
#' @param SCATTERMouseEnterColor Color when mouse entering the element
#' @param SCATTERMouseEnterCircleSize Circle size when mouse entering the element
#' @param SCATTERMouseEnterCircleOpacity Circle opacity when mouse entering the element
#' @param SCATTERMouseEnterCircleStrokeColor Circle stroke color when mouse entering the element
#' @param SCATTERMouseEnterCircleStrokeWidth Circle stroke width when mouse entering the element
#' @param SCATTERMouseLeaveDisplay Default False, show/not the tooltip when mouse mover leave a SCATTER point.
#' @param SCATTERMouseLeaveColor Color when mouse leaving the element
#' @param SCATTERMouseLeaveCircleSize Circle size when mouse leaving the element
#' @param SCATTERMouseLeaveCircleOpacity Circle opacity when mouse leaving the element
#' @param SCATTERMouseLeaveCircleStrokeColor Circle stroke color when mouse leaving the element
#' @param SCATTERMouseLeaveCircleStrokeWidth Circle stroke width when mouse leaving the element
#' @param SCATTERMouseMoveDisplay Default False, show/not the tooltip when mouse move into a SCATTER point.
#' @param SCATTERMouseMoveColor Color when mouse moving in the element
#' @param SCATTERMouseMoveCircleSize Circle size when mouse moving in the element
#' @param SCATTERMouseMoveCircleOpacity Circle opacity when mouse moving in the element
#' @param SCATTERMouseMoveCircleStrokeColor Circle stroke color when mouse moving in the element
#' @param SCATTERMouseMoveCircleStrokeWidth Circle stroke width when mouse moving in the element
#' @param SCATTERMouseOverDisplay Default False, show/not the tooltip when mouse hover on a SCATTER point.
#' @param SCATTERMouseOverColor Color when mouse moving over the element
#' @param SCATTERMouseOverCircleSize Circle size when mouse moving over the element
#' @param SCATTERMouseOverCircleOpacity Circle opacity when mouse moving over the element
#' @param SCATTERMouseOverCircleStrokeColor Circle stroke color when mouse moving over the element
#' @param SCATTERMouseOverCircleStrokeWidth Circle stroke width when mouse moving over the element
#' @param SCATTERMouseOverTooltipsSetting Default "style1"
#' @param SCATTERMouseOverTooltipsHtml Default " "
#' @param SCATTERMouseOverTooltipsPosition Default "absolute"
#' @param SCATTERMouseOverTooltipsBackgroundColor Default "white"
#' @param SCATTERMouseOverTooltipsBorderStyle Default "solid"
#' @param SCATTERMouseOverTooltipsBorderWidth Default 0
#' @param SCATTERMouseOverTooltipsPadding Default "3px"
#' @param SCATTERMouseOverTooltipsBorderRadius Default "3px"
#' @param SCATTERMouseOverTooltipsOpacity Default 0.8
#' @param SCATTERMouseOutDisplay Defalut False, hide/not tooltip when mouse is not hovering a SCATTER point anymore.
#' @param SCATTERMouseOutAnimationTime Animation time when mouse moving out the element
#' @param SCATTERMouseOutColor Color when mouse moving out the element
#' @param SCATTERMouseOutCircleSize Circle size when mouse moving out the element
#' @param SCATTERMouseOutCircleOpacity Circle opacity when mouse moving out the element
#' @param SCATTERMouseOutCircleStrokeColor Circle stroke color when mouse moving out the element
#' @param SCATTERMouseOutCircleStrokeWidth Circle stroke width when mouse moving out the element
#'
#' @param ARCxlink Default False, add/not xlink for ARC module
#' @param ARCMouseEvent Default True, open/not open mouse event of ARC module
#' @param ARCMouseClickDisplay Default False, show/not the tooltip when mouse click on a ARC point.
#' @param ARCMouseClickColor Color when mouse clicking the element
#' @param ARCMouseClickArcOpacity Arc opacity when mouse clicking the element
#' @param ARCMouseClickArcStrokeColor Arc stroke color when mouse clicking the element
#' @param ARCMouseClickArcStrokeWidth Arc stroke width when mouse clicking the element
#' @param ARCMouseClickTextFromData Text column when mouse clicking the element
#' @param ARCMouseClickTextOpacity Text opacity when mouse clicking the element
#' @param ARCMouseClickTextColor Text color when mouse clicking the element
#' @param ARCMouseClickTextSize Text size when mouse clicking the element
#' @param ARCMouseClickTextPostionX,ARCMouseClickTextPostionY Text coordinates when mouse clicking the element
#' @param ARCMouseClickTextDrag Whether text is draggable when mouse clicking the element
#' @param ARCMouseUpDisplay Default False, show/not the tooltip when mouse click up a ARC point.
#' @param ARCMouseUpColor Color when mouse moving up the element
#' @param ARCMouseUpArcOpacity Arc opacity when mouse moving up the element
#' @param ARCMouseUpArcStrokeColor Arc stroke color when mouse moving up the element
#' @param ARCMouseUpArcStrokeWidth Arc stroke width when mouse moving up the element
#' @param ARCMouseDownDisplay Default False, show/not the tooltip when mouse click down a ARC point.
#' @param ARCMouseDownColor Color when mouse moving down the element
#' @param ARCMouseDownArcOpacity Arc opacity when mouse moving down the element
#' @param ARCMouseDownArcStrokeColor Arc stroke color when mouse moving down the element
#' @param ARCMouseDownArcStrokeWidth Arc stroke width when mouse moving down the element
#' @param ARCMouseEnterDisplay Default False, show/not the tooltip when mouse mover over a ARC point.
#' @param ARCMouseEnterColor Color when mouse entering the element
#' @param ARCMouseEnterArcOpacity Arc opacity when mouse entering the element
#' @param ARCMouseEnterArcStrokeColor Arc stroke color when mouse entering the element
#' @param ARCMouseEnterArcStrokeWidth Arc stroke width when mouse entering the element
#' @param ARCMouseLeaveDisplay Default False, show/not the tooltip when mouse mover leave a ARC point.
#' @param ARCMouseLeaveColor Color when mouse leaving the element
#' @param ARCMouseLeaveArcOpacity Arc opacity when mouse leaving the element
#' @param ARCMouseLeaveArcStrokeColor Arc stroke color when mouse leaving the element
#' @param ARCMouseLeaveArcStrokeWidth Arc stroke width when mouse leaving the element
#' @param ARCMouseMoveDisplay Default False, show/not the tooltip when mouse move into a ARC point.
#' @param ARCMouseMoveColor Color when mouse moving in the element
#' @param ARCMouseMoveArcOpacity Arc opacity when mouse moving in the element
#' @param ARCMouseMoveArcStrokeColor Arc stroke color when mouse moving in the element
#' @param ARCMouseMoveArcStrokeWidth Arc stroke width when mouse moving in the element
#' @param ARCMouseOverDisplay Default False, show/not the tooltip when mouse hover on a ARC point.
#' @param ARCMouseOverColor Color when mouse moving over the element
#' @param ARCMouseOverArcOpacity Arc opacity when mouse moving over the element
#' @param ARCMouseOverArcStrokeColor Arc stroke color when mouse moving over the element
#' @param ARCMouseOverArcStrokeWidth Arc stroke width when mouse moving over the element
#' @param ARCMouseOverTooltipsSetting Default "style1"
#' @param ARCMouseOverTooltipsHtml Default " "
#' @param ARCMouseOverTooltipsPosition Default "absolute"
#' @param ARCMouseOverTooltipsBackgroundColor Default "white"
#' @param ARCMouseOverTooltipsBorderStyle Default "solid"
#' @param ARCMouseOverTooltipsBorderWidth Default 0
#' @param ARCMouseOverTooltipsPadding Default "3px"
#' @param ARCMouseOverTooltipsBorderRadius Default "3px"
#' @param ARCMouseOverTooltipsOpacity Default 0.8
#' @param ARCMouseOutDisplay Defalut False, hide/not tooltip when mouse is not hovering a ARC point anymore.
#' @param ARCMouseOutAnimationTime Animation time when mouse moving out the element
#' @param ARCMouseOutColor Color when mouse moving out the element
#' @param ARCMouseOutArcOpacity Arc opacity when mouse moving out the element
#' @param ARCMouseOutArcStrokeColor Arc stroke color when mouse moving out the element
#' @param ARCMouseOutArcStrokeWidth Arc stroke width when mouse moving out the element
#'
#' @param GENExlink Default False, add/not xlink for GENE module
#' @param GENEMouseEvent Default True, open/not open mouse event of GENE module
#' @param GENEMouseClickDisplay Default False, show/not the tooltip when mouse click on a GENE point.
#' @param GENEMouseClickColor Color when mouse clicking the element
#' @param GENEMouseClickArcOpacity Arc opacity when mouse clicking the element
#' @param GENEMouseClickArcStrokeColor Arc stroke color when mouse clicking the element
#' @param GENEMouseClickArcStrokeWidth Arc stroke width when mouse clicking the element
#' @param GENEMouseClickTextFromData Text column when mouse clicking the element
#' @param GENEMouseClickTextOpacity Text opacity when mouse clicking the element
#' @param GENEMouseClickTextColor Text color when mouse clicking the element
#' @param GENEMouseClickTextSize Text size when mouse clicking the element
#' @param GENEMouseClickTextPostionX,GENEMouseClickTextPostionY Text coordinates when mouse clicking the element
#' @param GENEMouseClickTextDrag Whether text is draggable when mouse clicking the element
#' @param GENEMouseUpDisplay Default False, show/not the tooltip when mouse click up a GENE point.
#' @param GENEMouseUpColor Color when mouse moving up the element
#' @param GENEMouseUpArcOpacity Arc opacity when mouse moving up the element
#' @param GENEMouseUpArcStrokeColor Arc stroke color when mouse moving up the element
#' @param GENEMouseUpArcStrokeWidth Arc stroke width when mouse moving up the element
#' @param GENEMouseDownDisplay Default False, show/not the tooltip when mouse click down a GENE point.
#' @param GENEMouseDownColor Color when mouse moving down the element
#' @param GENEMouseDownArcOpacity Arc opacity when mouse moving down the element
#' @param GENEMouseDownArcStrokeColor Arc stroke color when mouse moving down the element
#' @param GENEMouseDownArcStrokeWidth Arc stroke width when mouse moving down the element
#' @param GENEMouseEnterDisplay Default False, show/not the tooltip when mouse mover over a GENE point.
#' @param GENEMouseEnterColor Color when mouse entering the element
#' @param GENEMouseEnterArcOpacity Arc opacity when mouse entering the element
#' @param GENEMouseEnterArcStrokeColor Arc stroke color when mouse entering the element
#' @param GENEMouseEnterArcStrokeWidth Arc stroke width when mouse entering the element
#' @param GENEMouseLeaveDisplay Default False, show/not the tooltip when mouse mover leave a GENE point.
#' @param GENEMouseLeaveColor Color when mouse leaving the element
#' @param GENEMouseLeaveArcOpacity Arc opacity when mouse leaving the element
#' @param GENEMouseLeaveArcStrokeColor Arc stroke color when mouse leaving the element
#' @param GENEMouseLeaveArcStrokeWidth Arc stroke width when mouse leaving the element
#' @param GENEMouseMoveDisplay Default False, show/not the tooltip when mouse move into a GENE point.
#' @param GENEMouseMoveColor Color when mouse moving in the element
#' @param GENEMouseMoveArcOpacity Arc opacity when mouse moving in the element
#' @param GENEMouseMoveArcStrokeColor Arc stroke color when mouse moving in the element
#' @param GENEMouseMoveArcStrokeWidth Arc stroke width when mouse moving in the element
#' @param GENEMouseOverDisplay Default False, show/not the tooltip when mouse hover on a GENE point.
#' @param GENEMouseOverColor Color when mouse moving over the element
#' @param GENEMouseOverArcOpacity Arc opacity when mouse moving over the element
#' @param GENEMouseOverArcStrokeColor Arc stroke color when mouse moving over the element
#' @param GENEMouseOverArcStrokeWidth Arc stroke width when mouse moving over the element
#' @param GENEMouseOverTooltipsSetting Default "style1"
#' @param GENEMouseOverTooltipsHtml Default " "
#' @param GENEMouseOverTooltipsPosition Default "absolute"
#' @param GENEMouseOverTooltipsBackgroundColor Default "white"
#' @param GENEMouseOverTooltipsBorderStyle Default "solid"
#' @param GENEMouseOverTooltipsBorderWidth Default 0
#' @param GENEMouseOverTooltipsPadding Default "3px"
#' @param GENEMouseOverTooltipsBorderRadius Default "3px"
#' @param GENEMouseOverTooltipsOpacity Default 0.8
#' @param GENEMouseOutDisplay Defalut False, hide/not tooltip when mouse is not hovering a GENE point anymore.
#' @param GENEMouseOutAnimationTime Animation time when mouse moving out the element
#' @param GENEMouseOutColor Color when mouse moving out the element
#' @param GENEMouseOutArcOpacity Arc opacity when mouse moving out the element
#' @param GENEMouseOutArcStrokeColor Arc stroke color when mouse moving out the element
#' @param GENEMouseOutArcStrokeWidth Arc stroke width when mouse moving out the element
#'
#' @param LOLLIPOPxlink Default False, add/not xlink for LOLLIPOP module
#' @param LOLLIPOPMouseEvent Default True, open/not open mouse event of LOLLIPOP module
#' @param LOLLIPOPMouseClickDisplay Default False, show/not the tooltip when mouse click on a LOLLIPOP point.
#' @param LOLLIPOPMouseClickColor Color when mouse clicking
#' @param LOLLIPOPMouseClickCircleSize Circle size when mouse clicking the element
#' @param LOLLIPOPMouseClickCircleOpacity Circle opacity when mouse clicking the element
#' @param LOLLIPOPMouseClickCircleStrokeColor Circle stroke color when mouse clicking the element
#' @param LOLLIPOPMouseClickCircleStrokeWidth Circle stroke width when mouse clicking the element
#' @param LOLLIPOPMouseClickTextFromData Text column when mouse clicking the element
#' @param LOLLIPOPMouseClickTextOpacity Text opacity when mouse clicking the element
#' @param LOLLIPOPMouseClickTextColor Text color when mouse clicking the element
#' @param LOLLIPOPMouseClickTextSize Text size when mouse clicking the element
#' @param LOLLIPOPMouseClickTextPostionX,LOLLIPOPMouseClickTextPostionY Text coordinates when mouse clicking the element
#' @param LOLLIPOPMouseClickTextDrag Whether text is draggable when mouse clicking the element
#' @param LOLLIPOPMouseUpDisplay Default False, show/not the tooltip when mouse click up a LOLLIPOP point.
#' @param LOLLIPOPMouseUpColor Color when mouse moving up the element
#' @param LOLLIPOPMouseUpCircleSize Circle size when mouse moving up the element
#' @param LOLLIPOPMouseUpCircleOpacity Circle opacity when mouse moving up the element
#' @param LOLLIPOPMouseUpCircleStrokeColor Circle stroke color when mouse moving up the element
#' @param LOLLIPOPMouseUpCircleStrokeWidth Circle stroke width when mouse moving up the element
#' @param LOLLIPOPMouseDownDisplay Default False, show/not the tooltip when mouse click down a LOLLIPOP point.
#' @param LOLLIPOPMouseDownColor Color when mouse moving down the element
#' @param LOLLIPOPMouseDownCircleSize Circle size when mouse moving down the element
#' @param LOLLIPOPMouseDownCircleOpacity Circle opacity when mouse moving down the element
#' @param LOLLIPOPMouseDownCircleStrokeColor Circle stroke color when mouse moving down the element
#' @param LOLLIPOPMouseDownCircleStrokeWidth Circle stroke width when mouse moving down the element
#' @param LOLLIPOPMouseEnterDisplay Default False, show/not the tooltip when mouse mover over a LOLLIPOP point.
#' @param LOLLIPOPMouseEnterColor Color when mouse entering the element
#' @param LOLLIPOPMouseEnterCircleSize Circle size when mouse entering the element
#' @param LOLLIPOPMouseEnterCircleOpacity Circle opacity when mouse entering the element
#' @param LOLLIPOPMouseEnterCircleStrokeColor Circle stroke color when mouse entering the element
#' @param LOLLIPOPMouseEnterCircleStrokeWidth Circle stroke width when mouse entering the element
#' @param LOLLIPOPMouseLeaveDisplay Default False, show/not the tooltip when mouse mover leave a LOLLIPOP point.
#' @param LOLLIPOPMouseLeaveColor Color when mouse leaving the element
#' @param LOLLIPOPMouseLeaveCircleSize Circle size when mouse leaving the element
#' @param LOLLIPOPMouseLeaveCircleOpacity Circle opacity when mouse leaving the element
#' @param LOLLIPOPMouseLeaveCircleStrokeColor Circle stroke color when mouse leaving the element
#' @param LOLLIPOPMouseLeaveCircleStrokeWidth Circle stroke width when mouse leaving the element
#' @param LOLLIPOPMouseMoveDisplay Default False, show/not the tooltip when mouse move into a LOLLIPOP point.
#' @param LOLLIPOPMouseMoveColor Color when mouse moving in the element
#' @param LOLLIPOPMouseMoveCircleSize Circle size when mouse moving in the element
#' @param LOLLIPOPMouseMoveCircleOpacity Circle opacity when mouse moving in the element
#' @param LOLLIPOPMouseMoveCircleStrokeColor Circle stroke color when mouse moving in the element
#' @param LOLLIPOPMouseMoveCircleStrokeWidth Circle stroke width when mouse moving in the element
#' @param LOLLIPOPMouseOverDisplay Default False, show/not the tooltip when mouse hover on a LOLLIPOP point.
#' @param LOLLIPOPMouseOverColor Color when mouse moving over the element
#' @param LOLLIPOPMouseOverCircleSize Circle size when mouse moving over the element
#' @param LOLLIPOPMouseOverCircleOpacity Circle opacity when mouse moving over the element
#' @param LOLLIPOPMouseOverCircleStrokeColor Circle stroke color when mouse moving over the element
#' @param LOLLIPOPMouseOverCircleStrokeWidth Circle stroke width when mouse moving over the element
#' @param LOLLIPOPMouseOverTooltipsSetting Default "style1"
#' @param LOLLIPOPMouseOverTooltipsHtml Default " "
#' @param LOLLIPOPMouseOverTooltipsPosition Default "absolute"
#' @param LOLLIPOPMouseOverTooltipsBackgroundColor Default "white"
#' @param LOLLIPOPMouseOverTooltipsBorderStyle Default "solid"
#' @param LOLLIPOPMouseOverTooltipsBorderWidth Default 0
#' @param LOLLIPOPMouseOverTooltipsPadding Default "3px"
#' @param LOLLIPOPMouseOverTooltipsBorderRadius Default "3px"
#' @param LOLLIPOPMouseOverTooltipsOpacity Default 0.8
#' @param LOLLIPOPMouseOutDisplay Defalut False, hide/not tooltip when mouse is not hovering a LOLLIPOP point anymore.
#' @param LOLLIPOPMouseOutAnimationTime Animation time when mouse moving out the element
#' @param LOLLIPOPMouseOutColor Color when mouse moving out the element
#' @param LOLLIPOPMouseOutCircleSize Circle size when mouse moving out the element
#' @param LOLLIPOPMouseOutCircleOpacity Circle opacity when mouse moving out the element
#' @param LOLLIPOPMouseOutCircleStrokeColor Circle stroke color when mouse moving out the element
#' @param LOLLIPOPMouseOutCircleStrokeWidth Circle stroke width when mouse moving out the element
#'
#' @param CNVxlink Default False, add/not xlink for CNV module
#' @param CNVMouseEvent Default True, open/not open mouse event of CNV module
#' @param CNVMouseClickDisplay Default False, show/not the tooltip when mouse click on a CNV point.
#' @param CNVMouseClickColor Color when mouse clicking
#' @param CNVMouseClickArcOpacity Arc opacity when mouse clicking the element
#' @param CNVMouseClickArcStrokeColor Arc stroke color when mouse clicking the element
#' @param CNVMouseClickArcStrokeWidth Arc stroke width when mouse clicking the element
#' @param CNVMouseClickTextFromData Text column when mouse clicking the element
#' @param CNVMouseClickTextOpacity Text opacity when mouse clicking the element
#' @param CNVMouseClickTextColor Text color when mouse clicking the element
#' @param CNVMouseClickTextSize Text size when mouse clicking the element
#' @param CNVMouseClickTextPostionX,CNVMouseClickTextPostionY Text coordinates when mouse clicking the element
#' @param CNVMouseClickTextDrag Whether text is draggable when mouse clicking the element
#' @param CNVMouseUpDisplay Default False, show/not the tooltip when mouse click up a CNV point.
#' @param CNVMouseUpColor Color when mouse moving up the element
#' @param CNVMouseUpArcOpacity Arc opacity when mouse clicking the element
#' @param CNVMouseUpArcStrokeColor Arc stroke color when mouse clicking the element
#' @param CNVMouseUpArcStrokeWidth Arc stroke width when mouse clicking the element
#' @param CNVMouseDownDisplay Default False, show/not the tooltip when mouse click down a CNV point.
#' @param CNVMouseDownColor Color when mouse moving down the element
#' @param CNVMouseDownArcOpacity Arc opacity when mouse moving down the element
#' @param CNVMouseDownArcStrokeColor Arc stroke color when mouse moving down the element
#' @param CNVMouseDownArcStrokeWidth Arc stroke width when mouse moving down the element
#' @param CNVMouseEnterDisplay Default False, show/not the tooltip when mouse mover over a CNV point.
#' @param CNVMouseEnterColor Color when mouse entering the element
#' @param CNVMouseEnterArcOpacity Arc opacity when mouse entering the element
#' @param CNVMouseEnterArcStrokeColor Arc stroke color when mouse entering the element
#' @param CNVMouseEnterArcStrokeWidth Arc stroke width when mouse entering the element
#' @param CNVMouseLeaveDisplay Default False, show/not the tooltip when mouse mover leave a CNV point.
#' @param CNVMouseLeaveColor Color when mouse leaving the element
#' @param CNVMouseLeaveArcOpacity Arc opacity when mouse leaving the element
#' @param CNVMouseLeaveArcStrokeColor Arc stroke color when mouse leaving the element
#' @param CNVMouseLeaveArcStrokeWidth Arc stroke width when mouse leaving the element
#' @param CNVMouseMoveDisplay Default False, show/not the tooltip when mouse move into a CNV point.
#' @param CNVMouseMoveColor Color when mouse moving in the element
#' @param CNVMouseMoveArcOpacity Arc opacity when mouse moving in the element
#' @param CNVMouseMoveArcStrokeColor Arc stroke color when mouse moving in the element
#' @param CNVMouseMoveArcStrokeWidth Arc stroke width when mouse moving in the element
#' @param CNVMouseOverDisplay Default False, show/not the tooltip when mouse hover on a CNV point.
#' @param CNVMouseOverColor Color when mouse moving over the element
#' @param CNVMouseOverArcOpacity Arc opacity when mouse moving over the element
#' @param CNVMouseOverArcStrokeColor Arc stroke color when mouse moving over the element
#' @param CNVMouseOverArcStrokeWidth Arc stroke width when mouse moving over the element
#' @param CNVMouseOverTooltipsSetting Default "style1"
#' @param CNVMouseOverTooltipsHtml Default " "
#' @param CNVMouseOverTooltipsPosition Default "absolute"
#' @param CNVMouseOverTooltipsBackgroundColor Default "white"
#' @param CNVMouseOverTooltipsBorderStyle Default "solid"
#' @param CNVMouseOverTooltipsBorderWidth Default 0
#' @param CNVMouseOverTooltipsPadding Default "3px"
#' @param CNVMouseOverTooltipsBorderRadius Default "3px"
#' @param CNVMouseOverTooltipsOpacity Default 0.8
#' @param CNVMouseOutDisplay Defalut False, hide/not tooltip when mouse is not hovering a CNV point anymore.
#' @param CNVMouseOutAnimationTime Animation time when mouse moving out the element
#' @param CNVMouseOutColor Color when mouse moving out the element
#' @param CNVMouseOutArcOpacity Arc opacity when mouse moving out the element
#' @param CNVMouseOutArcStrokeColor Arc stroke color when mouse moving out the element
#' @param CNVMouseOutArcStrokeWidth Arc stroke width when mouse moving out the element
#'
#' @param HEATMAPMouseEvent Default True, open/not open mouse event of HEATMAP module
#' @param HEATMAPMouseClickDisplay Default False, show/not the tooltip when mouse click on a HEATMAP point.
#' @param HEATMAPMouseClickColor Color when mouse clicking
#' @param HEATMAPMouseClickOpacity Opacity when mouse clicking
#' @param HEATMAPMouseClickStrokeColor Stroke color when mouse clicking
#' @param HEATMAPMouseClickStrokeWidth Stroke width when mouse clicking
#' @param HEATMAPMouseUpDisplay Default False, show/not the tooltip when mouse click up a HEATMAP point.
#' @param HEATMAPMouseUpColor Color when mouse moving up the element
#' @param HEATMAPMouseUpOpacity Opacity when mouse moving up the element
#' @param HEATMAPMouseUpStrokeColor Stroke color when mouse moving up the element
#' @param HEATMAPMouseUpStrokeWidth Stroke width when mouse moving up the element
#' @param HEATMAPMouseDownDisplay Default False, show/not the tooltip when mouse click down a HEATMAP point.
#' @param HEATMAPMouseDownColor Color when mouse moving down the element
#' @param HEATMAPMouseDownOpacity Opacity when mouse moving down the element
#' @param HEATMAPMouseDownStrokeColor Stroke color when mouse moving down the element
#' @param HEATMAPMouseDownStrokeWidth Stroke width when mouse moving down the element
#' @param HEATMAPMouseEnterDisplay Default False, show/not the tooltip when mouse mover over a HEATMAP point.
#' @param HEATMAPMouseEnterColor Color when mouse entering the element
#' @param HEATMAPMouseEnterOpacity Opacity when mouse entering the element
#' @param HEATMAPMouseEnterStrokeColor Stroke color when mouse entering the element
#' @param HEATMAPMouseEnterStrokeWidth Stroke width when mouse entering the element
#' @param HEATMAPMouseLeaveDisplay Default False, show/not the tooltip when mouse mover leave a HEATMAP point.
#' @param HEATMAPMouseLeaveColor Color when mouse leaving the element
#' @param HEATMAPMouseLeaveOpacity Opacity when mouse leaving the element
#' @param HEATMAPMouseLeaveStrokeColor Stroke color when mouse leaving the element
#' @param HEATMAPMouseLeaveStrokeWidth Stroke width when mouse leaving the element
#' @param HEATMAPMouseMoveDisplay Default False, show/not the tooltip when mouse move into a HEATMAP point.
#' @param HEATMAPMouseMoveColor Color when mouse moving in the element
#' @param HEATMAPMouseMoveOpacity Opacity when mouse moving in the element
#' @param HEATMAPMouseMoveStrokeColor Stroke color when mouse moving in the element
#' @param HEATMAPMouseMoveStrokeWidth Stroke width when mouse moving in the element
#' @param HEATMAPMouseOverDisplay Default False, show/not the tooltip when mouse hover on a HEATMAP point.
#' @param HEATMAPMouseOverColor Color when mouse moving over the element
#' @param HEATMAPMouseOverOpacity Opacity when mouse moving over the element
#' @param HEATMAPMouseOverStrokeColor Stroke color when mouse moving over the element
#' @param HEATMAPMouseOverStrokeWidth Stroke width when mouse moving over the element
#' @param HEATMAPMouseOverTooltipsSetting Default "style1"
#' @param HEATMAPMouseOverTooltipsHtml Default " "
#' @param HEATMAPMouseOverTooltipsPosition Default "absolute"
#' @param HEATMAPMouseOverTooltipsBackgroundColor Default "white"
#' @param HEATMAPMouseOverTooltipsBorderStyle Default "solid"
#' @param HEATMAPMouseOverTooltipsBorderWidth Default 0
#' @param HEATMAPMouseOverTooltipsPadding Default "3px"
#' @param HEATMAPMouseOverTooltipsBorderRadius Default "3px"
#' @param HEATMAPMouseOverTooltipsOpacity Default 0.8
#' @param HEATMAPMouseOutDisplay Defalut False, hide/not tooltip when mouse is not hovering a HEATMAP point anymore.
#' @param HEATMAPMouseOutAnimationTime Animation time when mouse moving out the element
#' @param HEATMAPMouseOutColor Color when mouse moving out the element
#' @param HEATMAPMouseOutOpacity Opacity when mouse moving out the element
#' @param HEATMAPMouseOutStrokeColor Stroke color when mouse moving out the element
#' @param HEATMAPMouseOutStrokeWidth Stroke width when mouse moving out the element
#'
#' @param BUBBLExlink Default False, add/not xlink for BUBBLE module
#' @param BUBBLEMouseEvent Default True, open/not open mouse event of BUBBLE module
#' @param BUBBLEMouseClickDisplay Default False, show/not the tooltip when mouse click on a BUBBLE point.
#' @param BUBBLEMouseClickColor Color when mouse clicking
#' @param BUBBLEMouseClickOpacity Opacity when mouse clicking
#' @param BUBBLEMouseClickStrokeColor Stroke color when mouse clicking
#' @param BUBBLEMouseClickStrokeWidth Stroke width when mouse clicking
#' @param BUBBLEMouseUpDisplay Default False, show/not the tooltip when mouse click up a BUBBLE point.
#' @param BUBBLEMouseUpColor Color when mouse moving up the element
#' @param BUBBLEMouseUpOpacity Opacity when mouse moving up the element
#' @param BUBBLEMouseUpStrokeColor Stroke color when mouse moving up the element
#' @param BUBBLEMouseUpStrokeWidth Stroke width when mouse moving up the element
#' @param BUBBLEMouseDownDisplay Default False, show/not the tooltip when mouse click down a BUBBLE point.
#' @param BUBBLEMouseDownColor Color when mouse moving down the element
#' @param BUBBLEMouseDownOpacity Opacity when mouse moving down the element
#' @param BUBBLEMouseDownStrokeColor Stroke color when mouse moving down the element
#' @param BUBBLEMouseDownStrokeWidth Stroke width when mouse moving down the element
#' @param BUBBLEMouseEnterDisplay Default False, show/not the tooltip when mouse mover over a BUBBLE point.
#' @param BUBBLEMouseEnterColor Color when mouse entering the element
#' @param BUBBLEMouseEnterOpacity Opacity when mouse entering the element
#' @param BUBBLEMouseEnterStrokeColor Stroke color when mouse entering the element
#' @param BUBBLEMouseEnterStrokeWidth Stroke width when mouse entering the element
#' @param BUBBLEMouseLeaveDisplay Default False, show/not the tooltip when mouse mover leave a BUBBLE point.
#' @param BUBBLEMouseLeaveColor Color when mouse leaving the element
#' @param BUBBLEMouseLeaveOpacity Opacity when mouse leaving the element
#' @param BUBBLEMouseLeaveStrokeColor Stroke color when mouse leaving the element
#' @param BUBBLEMouseLeaveStrokeWidth Stroke width when mouse leaving the element
#' @param BUBBLEMouseMoveDisplay Default False, show/not the tooltip when mouse move into a BUBBLE point.
#' @param BUBBLEMouseMoveColor Color when mouse moving in the element
#' @param BUBBLEMouseMoveOpacity Opacity when mouse moving in the element
#' @param BUBBLEMouseMoveStrokeColor Stroke color when mouse moving in the element
#' @param BUBBLEMouseMoveStrokeWidth Stroke width when mouse moving in the element
#' @param BUBBLEMouseOverDisplay Default False, show/not the tooltip when mouse hover on a BUBBLE point.
#' @param BUBBLEMouseOverColor Color when mouse moving over the element
#' @param BUBBLEMouseOverOpacity Opacity when mouse moving over the element
#' @param BUBBLEMouseOverStrokeColor Stroke color when mouse moving over the element
#' @param BUBBLEMouseOverStrokeWidth Stroke width when mouse moving over the element
#' @param BUBBLEMouseOverTooltipsSetting Default "style1"
#' @param BUBBLEMouseOverTooltipsHtml Default " "
#' @param BUBBLEMouseOverTooltipsPosition Default "absolute"
#' @param BUBBLEMouseOverTooltipsBackgroundColor Default "white"
#' @param BUBBLEMouseOverTooltipsBorderStyle Default "solid"
#' @param BUBBLEMouseOverTooltipsBorderWidth Default 0
#' @param BUBBLEMouseOverTooltipsPadding Default "3px"
#' @param BUBBLEMouseOverTooltipsBorderRadius Default "3px"
#' @param BUBBLEMouseOverTooltipsOpacity Default 0.8
#' @param BUBBLEMouseOutDisplay Defalut False, hide/not tooltip when mouse is not hovering a BUBBLE point anymore.
#' @param BUBBLEMouseOutAnimationTime Animation time when mouse moving out the element
#' @param BUBBLEMouseOutColor Color when mouse moving out the element
#' @param BUBBLEMouseOutOpacity Opacity when mouse moving out the element
#' @param BUBBLEMouseOutStrokeColor Stroke color when mouse moving out the element
#' @param BUBBLEMouseOutStrokeWidth Stroke width when mouse moving out the element
#'
#' @param elementId the name of the HTML id to be used to contain the visualization.
#'
#' @param ... Ignored
#'
#' @examples
#' NGCircos(genome = "hg19")
#'
#' @export
NGCircos <- function(tracklist = NGCircosTracklist(),
                     genome = "hg19", genome2 = "hg19", genomeFillColor = "Spectral", chrPad = 0.02, width = NULL, height = NULL,
                     innerRadius = 216, outerRadius=240, svgClassName = "NGCircos",
                     displayGenomeBorder = TRUE, genomeBorderColor = "#000", genomeBorderSize = 0.5,
                     genomeTicksDisplay = FALSE, genomeTicksLen = 5, genomeTicksColor = "#000", genomeTicksTextSize = "0.6em",
                     genomeTicksRealLength = TRUE, genomeTicksTextColor = "#000", genomeTicksScale = 30000000, genomeTicksOffset = 0,
                     genomeLabelDisplay = TRUE, genomeLabelTextSize = "10pt", genomeLabelTextColor = "#000",
                     genomeLabelDx = 0, genomeLabelDy = 0, compareEvent=FALSE,compareEventGroupGapRate=0.1,compareEventGroupDistance=0,
                     zoom = TRUE, TEXTModuleDragEvent = FALSE,
                     CNVxlink=FALSE, CNVMouseEvent = TRUE, CNVMouseClickDisplay = FALSE, CNVMouseClickColor = "red",
                     CNVMouseClickArcOpacity = 1, CNVMouseClickArcStrokeColor = "#F26223",
                     CNVMouseClickArcStrokeWidth = 0, CNVMouseClickTextFromData = "fourth",
                     CNVMouseClickTextOpacity = 1, CNVMouseClickTextColor = "red",
                     CNVMouseClickTextSize = 8, CNVMouseClickTextPostionX = 0, CNVMouseClickTextPostionY = 0,
                     CNVMouseClickTextDrag = TRUE, CNVMouseDownDisplay = FALSE, CNVMouseDownColor = "green",
                     CNVMouseDownArcOpacity = 1, CNVMouseDownArcStrokeColor = "#F26223", CNVMouseDownArcStrokeWidth = 0,
                     CNVMouseEnterDisplay = FALSE, CNVMouseEnterColor = "yellow", CNVMouseEnterArcOpacity = 1,
                     CNVMouseEnterArcStrokeColor = "#F26223", CNVMouseEnterArcStrokeWidth = 0,
                     CNVMouseLeaveDisplay = FALSE, CNVMouseLeaveColor = "pink", CNVMouseLeaveArcOpacity = 1,
                     CNVMouseLeaveArcStrokeColor = "#F26223", CNVMouseLeaveArcStrokeWidth = 0,
                     CNVMouseMoveDisplay = FALSE, CNVMouseMoveColor = "red", CNVMouseMoveArcOpacity = 1,
                     CNVMouseMoveArcStrokeColor = "#F26223", CNVMouseMoveArcStrokeWidth = 0,
                     CNVMouseOutDisplay = FALSE, CNVMouseOutAnimationTime = 500, CNVMouseOutColor = "red",
                     CNVMouseOutArcOpacity =1, CNVMouseOutArcStrokeColor = "red", CNVMouseOutArcStrokeWidth = 0,
                     CNVMouseUpDisplay = FALSE, CNVMouseUpColor = "grey", CNVMouseUpArcOpacity = 1,
                     CNVMouseUpArcStrokeColor = "#F26223", CNVMouseUpArcStrokeWidth = 0,
                     CNVMouseOverDisplay = FALSE, CNVMouseOverColor = "red", CNVMouseOverArcOpacity = 1,
                     CNVMouseOverArcStrokeColor = "#F26223", CNVMouseOverArcStrokeWidth = 3,
                     CNVMouseOverTooltipsSetting = "style1",CNVMouseOverTooltipsHtml = " ",
                     CNVMouseOverTooltipsPosition = "absolute", CNVMouseOverTooltipsBackgroundColor = "white",
                     CNVMouseOverTooltipsBorderStyle = "solid", CNVMouseOverTooltipsBorderWidth = 0,
                     CNVMouseOverTooltipsPadding = "3px", CNVMouseOverTooltipsBorderRadius = "3px",
                     CNVMouseOverTooltipsOpacity = 0.8,
                     HEATMAPMouseEvent = TRUE, HEATMAPMouseClickDisplay = FALSE, HEATMAPMouseClickColor = "green",
                     HEATMAPMouseClickOpacity = 1, HEATMAPMouseClickStrokeColor = "none",
                     HEATMAPMouseClickStrokeWidth = "none", HEATMAPMouseDownDisplay = FALSE,
                     HEATMAPMouseDownColor = "green", HEATMAPMouseDownOpacity = 1, HEATMAPMouseDownStrokeColor = "none",
                     HEATMAPMouseDownStrokeWidth = "none", HEATMAPMouseEnterDisplay = FALSE,
                     HEATMAPMouseEnterColor = "green", HEATMAPMouseEnterOpacity = 1, HEATMAPMouseEnterStrokeColor = "none",
                     HEATMAPMouseEnterStrokeWidth = "none", HEATMAPMouseLeaveDisplay = FALSE,
                     HEATMAPMouseLeaveColor = "green", HEATMAPMouseLeaveOpacity = 1, HEATMAPMouseLeaveStrokeColor = "none",
                     HEATMAPMouseLeaveStrokeWidth = "none", HEATMAPMouseMoveDisplay = FALSE,
                     HEATMAPMouseMoveColor = "green", HEATMAPMouseMoveOpacity = 1, HEATMAPMouseMoveStrokeColor = "none",
                     HEATMAPMouseMoveStrokeWidth = "none", HEATMAPMouseOutDisplay = FALSE,
                     HEATMAPMouseOutAnimationTime = 500, HEATMAPMouseOutColor = "green",
                     HEATMAPMouseOutOpacity = 1, HEATMAPMouseOutStrokeColor = "none",
                     HEATMAPMouseOutStrokeWidth = "none", HEATMAPMouseUpDisplay = FALSE, HEATMAPMouseUpColor = "green",
                     HEATMAPMouseUpOpacity = 1, HEATMAPMouseUpStrokeColor = "none", HEATMAPMouseUpStrokeWidth = "none",
                     HEATMAPMouseOverDisplay = FALSE, HEATMAPMouseOverColor = "none", HEATMAPMouseOverOpacity = 1,
                     HEATMAPMouseOverStrokeColor = "none", HEATMAPMouseOverStrokeWidth = "none",
                     HEATMAPMouseOverTooltipsSetting = "style1", HEATMAPMouseOverTooltipsHtml = " ",
                     HEATMAPMouseOverTooltipsPosition = "absolute", HEATMAPMouseOverTooltipsBackgroundColor = "white",
                     HEATMAPMouseOverTooltipsBorderStyle = "solid", HEATMAPMouseOverTooltipsBorderWidth = 0,
                     HEATMAPMouseOverTooltipsPadding = "3px", HEATMAPMouseOverTooltipsBorderRadius = "3px",
                     HEATMAPMouseOverTooltipsOpacity = 0.8,
                     BUBBLExlink = FALSE, BUBBLEMouseEvent = TRUE, BUBBLEMouseClickDisplay = FALSE,
                     BUBBLEMouseClickColor = "green", BUBBLEMouseClickOpacity = 1, BUBBLEMouseClickStrokeColor = "none",
                     BUBBLEMouseClickStrokeWidth = "none", BUBBLEMouseDownDisplay = FALSE, BUBBLEMouseDownColor = "green",
                     BUBBLEMouseDownOpacity = 1, BUBBLEMouseDownStrokeColor = "none", BUBBLEMouseDownStrokeWidth = "none",
                     BUBBLEMouseEnterDisplay = FALSE, BUBBLEMouseEnterColor = "green", BUBBLEMouseEnterOpacity = 1,
                     BUBBLEMouseEnterStrokeColor = "none", BUBBLEMouseEnterStrokeWidth = "none",
                     BUBBLEMouseLeaveDisplay = FALSE, BUBBLEMouseLeaveColor = "green", BUBBLEMouseLeaveOpacity = 1,
                     BUBBLEMouseLeaveStrokeColor = "none", BUBBLEMouseLeaveStrokeWidth = "none",
                     BUBBLEMouseMoveDisplay = FALSE, BUBBLEMouseMoveColor = "green", BUBBLEMouseMoveOpacity = 1,
                     BUBBLEMouseMoveStrokeColor = "none", BUBBLEMouseMoveStrokeWidth = "none", BUBBLEMouseOutDisplay = FALSE,
                     BUBBLEMouseOutAnimationTime = 500, BUBBLEMouseOutColor = "green", BUBBLEMouseOutOpacity = 1,
                     BUBBLEMouseOutStrokeColor = "none", BUBBLEMouseOutStrokeWidth = "none",
                     BUBBLEMouseUpDisplay = FALSE, BUBBLEMouseUpColor = "green", BUBBLEMouseUpOpacity = 1,
                     BUBBLEMouseUpStrokeColor = "none", BUBBLEMouseUpStrokeWidth = "none",
                     BUBBLEMouseOverDisplay = FALSE, BUBBLEMouseOverColor = "green", BUBBLEMouseOverOpacity = 1,
                     BUBBLEMouseOverStrokeColor = "none", BUBBLEMouseOverStrokeWidth = "none",
                     BUBBLEMouseOverTooltipsSetting = "style1", BUBBLEMouseOverTooltipsHtml = " ",
                     BUBBLEMouseOverTooltipsPosition = "absolute", BUBBLEMouseOverTooltipsBackgroundColor = "white",
                     BUBBLEMouseOverTooltipsBorderStyle = "solid", BUBBLEMouseOverTooltipsBorderWidth = 0,
                     BUBBLEMouseOverTooltipsPadding = "3px", BUBBLEMouseOverTooltipsBorderRadius = "3px",
                     BUBBLEMouseOverTooltipsOpacity = 0.8,
                     SNPxlink = FALSE, SNPMouseEvent = TRUE, SNPMouseCombinationEvent = FALSE,
                     SNPMouseCombinationImageDisplay = FALSE, SNPMouseCombinationImageTitle = "This is image",
                     SNPMouseCombinationImageTitleSize = 5, SNPMouseCombinationImageTitleWeight = "bold",
                     SNPMouseCombinationImageTitleColor = "black", SNPMouseCombinationImagePositionX = 0,
                     SNPMouseCombinationImagePositionY = 0, SNPMouseCombinationImageHeight = 200,
                     SNPMouseCombinationImageWidth = 300, SNPMouseCombinationGraphDisplay = FALSE,
                     SNPMouseCombinationGraphTitle = "This is graph", SNPMouseCombinationGraphTitleSize = 5,
                     SNPMouseCombinationGraphTitleWeight = "bold", SNPMouseCombinationGraphTitleColor = "black",
                     SNPMouseCombinationGraphType = "histogram", SNPMouseCombinationGraphPositionX = 0,
                     SNPMouseCombinationGraphPositionY = 0, SNPMouseCombinationGraphHeight = 200,
                     SNPMouseCombinationGraphWidth = 300, SNPMouseCombinationGraphHistogramBarColor = "blue",
                     SNPMouseCombinationGraphHistogramPadding = 30, SNPMouseCombinationGraphHistogramPositionCorrectX = 0,
                     SNPMouseCombinationGraphPieAutoColor = TRUE, SNPMouseCombinationGraphPieColor = c("blue","orange"),
                     SNPMouseCombinationGraphPieSize = 50, SNPMouseCombinationGraphPieStroke = TRUE,
                     SNPMouseCombinationGraphPieStrokeColor = "black", SNPMouseCombinationGraphPieStrokeWidth = 1,
                     SNPMouseCombinationGraphPieOpacity = 1, SNPMouseCombinationGraphLineType = "linear",
                     SNPMouseCombinationGraphLineColor = "black", SNPMouseCombinationGraphLineWidth = 1,
                     SNPMouseCombinationGraphLinePoint = FALSE, SNPMouseCombinationGraphLinePointSize = 5,
                     SNPMouseCombinationGraphLinePointAutoColor = TRUE, SNPMouseCombinationGraphLinePointColor = c("blue","orange"),
                     SNPMouseCombinationGraphLinePointStroke = TRUE, SNPMouseCombinationGraphLinePointStrokeColor = "black",
                     SNPMouseCombinationGraphLinePointStrokeWidth = 1, SNPMouseCombinationGraphLinePointOpacity = 1,
                     SNPMouseCombinationGraphLinePositionCorrectX = 0, SNPMouseCombinationTextDisplay = FALSE,
                     SNPMouseCombinationTextColor = "red", SNPMouseCombinationTextSize = 3, SNPMouseCombinationTextWeight = "bold",
                     SNPMouseCombinationTextPositionCorrectX = 0, SNPMouseCombinationTextPositionCorrectY = 0,
                     SNPMouseClickDisplay = FALSE, SNPMouseClickColor = "red", SNPMouseClickCircleSize = 4,
                     SNPMouseClickCircleOpacity = 1, SNPMouseClickCircleStrokeColor = "#F26223",
                     SNPMouseClickCircleStrokeWidth = 0, SNPMouseClickTextFromData = "fourth", SNPMouseClickTextOpacity = 1,
                     SNPMouseClickTextColor = "red", SNPMouseClickTextSize = 8, SNPMouseClickTextPostionX = 1,
                     SNPMouseClickTextPostionY = 10, SNPMouseClickTextDrag = TRUE, SNPMouseDownDisplay = FALSE,
                     SNPMouseDownColor = "green", SNPMouseDownCircleSize = 4, SNPMouseDownCircleOpacity = 1,
                     SNPMouseDownCircleStrokeColor = "#F26223", SNPMouseDownCircleStrokeWidth = 0,
                     SNPMouseEnterDisplay = FALSE, SNPMouseEnterColor = "yellow", SNPMouseEnterCircleSize = 4,
                     SNPMouseEnterCircleOpacity = 1, SNPMouseEnterCircleStrokeColor = "#F26223", SNPMouseEnterCircleStrokeWidth = 0,
                     SNPMouseLeaveDisplay = FALSE, SNPMouseLeaveColor = "pink", SNPMouseLeaveCircleSize = 4,
                     SNPMouseLeaveCircleOpacity = 1, SNPMouseLeaveCircleStrokeColor = "#F26223", SNPMouseLeaveCircleStrokeWidth = 0,
                     SNPMouseMoveDisplay = FALSE, SNPMouseMoveColor = "red", SNPMouseMoveCircleSize = 2,
                     SNPMouseMoveCircleOpacity = 1, SNPMouseMoveCircleStrokeColor = "#F26223", SNPMouseMoveCircleStrokeWidth = 0,
                     SNPMouseOutDisplay = FALSE, SNPMouseOutAnimationTime = 500, SNPMouseOutColor = "red",
                     SNPMouseOutCircleSize = 2, SNPMouseOutCircleOpacity = 1, SNPMouseOutCircleStrokeColor = "red",
                     SNPMouseOutCircleStrokeWidth = 0, SNPMouseUpDisplay = FALSE, SNPMouseUpColor = "grey",
                     SNPMouseUpCircleSize = 2, SNPMouseUpCircleOpacity = 1, SNPMouseUpCircleStrokeColor = "#F26223",
                     SNPMouseUpCircleStrokeWidth = 0, SNPMouseOverDisplay = FALSE, SNPMouseOverColor = "red",
                     SNPMouseOverCircleSize = 2, SNPMouseOverCircleOpacity = 1, SNPMouseOverCircleStrokeColor = "#F26223",
                     SNPMouseOverCircleStrokeWidth = 3, SNPMouseOverTooltipsSetting = "style1", SNPMouseOverTooltipsHtml = " ",
                     SNPMouseOverTooltipsPosition = "absolute", SNPMouseOverTooltipsBackgroundColor = "white",
                     SNPMouseOverTooltipsBorderStyle = "solid", SNPMouseOverTooltipsBorderWidth = 0,
                     SNPMouseOverTooltipsPadding = "3px", SNPMouseOverTooltipsBorderRadius = "3px",
                     SNPMouseOverTooltipsOpacity = 0.8,
                     LINKxlink = FALSE, LINKMouseEvent = TRUE, LINKMouseClickDisplay = FALSE, LINKMouseClickOpacity = 1,
                     LINKMouseClickStrokeColor = "green", LINKMouseClickStrokeWidth = 4, LINKMouseDownDisplay = FALSE,
                     LINKMouseDownOpacity = 1, LINKMouseDownStrokeColor = "none", LINKMouseDownStrokeWidth = "none",
                     LINKMouseEnterDisplay = FALSE, LINKMouseEnterOpacity = 1, LINKMouseEnterStrokeColor = "none",
                     LINKMouseEnterStrokeWidth = "none", LINKMouseLeaveDisplay = FALSE, LINKMouseLeaveOpacity = 1,
                     LINKMouseLeaveStrokeColor = "none", LINKMouseLeaveStrokeWidth = "none",
                     LINKMouseMoveDisplay = FALSE, LINKMouseMoveOpacity = 1, LINKMouseMoveStrokeColor = "none",
                     LINKMouseMoveStrokeWidth = "none", LINKMouseOutDisplay = FALSE, LINKMouseOutAnimationTime = 500,
                     LINKMouseOutOpacity = 1, LINKMouseOutStrokeColor = "none", LINKMouseOutStrokeWidth = "none",
                     LINKMouseUpDisplay = FALSE, LINKMouseUpOpacity = 1, LINKMouseUpStrokeColor = "none",
                     LINKMouseUpStrokeWidth = "none", LINKMouseOverDisplay = FALSE, LINKMouseOverOpacity = 1,
                     LINKMouseOverStrokeColor = "none", LINKMouseOverStrokeWidth = "none",
                     LINKMouseOverTooltipsSetting = "style1", LINKMouseOverTooltipsHtml = " ",
                     LINKMouseOverTooltipsPosition = "absolute", LINKMouseOverTooltipsBackgroundColor = "white",
                     LINKMouseOverTooltipsBorderStyle = "solid", LINKMouseOverTooltipsBorderWidth = 0,
                     LINKMouseOverTooltipsPadding = "3px", LINKMouseOverTooltipsBorderRadius = "3px",
                     LINKMouseOverTooltipsOpacity = 1, LINKLabelDragEvent = FALSE,
                     CHORDMouseEvent = TRUE, CHORDMouseFillColorExcluded = "#FFFFFF", CHORDMouseClickDisplay = FALSE,
                     CHORDMouseClickOpacity = 1, CHORDMouseClickStrokeColor = "none", CHORDMouseClickStrokeWidth = "none",
                     CHORDMouseDownDisplay = FALSE, CHORDMouseDownOpacity = 1, CHORDMouseDownStrokeColor = "none",
                     CHORDMouseDownStrokeWidth = "none", CHORDMouseEnterDisplay = FALSE,
                     CHORDMouseEnterOpacity = 1, CHORDMouseEnterStrokeColor = "none", CHORDMouseEnterStrokeWidth = "none",
                     CHORDMouseLeaveDisplay = FALSE, CHORDMouseLeaveOpacity = 1,
                     CHORDMouseLeaveStrokeColor = "none", CHORDMouseLeaveStrokeWidth = "none",
                     CHORDMouseMoveDisplay = FALSE, CHORDMouseMoveOpacity = 1,
                     CHORDMouseMoveStrokeColor = "none", CHORDMouseMoveStrokeWidth = "none",
                     CHORDMouseOutDisplay = FALSE, CHORDMouseOutAnimationTime = 500, CHORDMouseOutOpacity = 1,
                     CHORDMouseOutStrokeColor = "none", CHORDMouseOutStrokeWidth = "none",
                     CHORDMouseUpDisplay = FALSE, CHORDMouseUpOpacity = 1,
                     CHORDMouseUpStrokeColor = "none", CHORDMouseUpStrokeWidth = "none",
                     CHORDMouseOverDisplay = FALSE, CHORDMouseOverOpacity = 1,
                     CHORDMouseOverStrokeColor = "none", CHORDMouseOverStrokeWidth = "none",
                     HISTOGRAMxlink = FALSE, HISTOGRAMMouseEvent = TRUE,
                     HISTOGRAMMouseClickDisplay = FALSE, HISTOGRAMMouseClickColor = "red",
                     HISTOGRAMMouseClickOpacity = 1, HISTOGRAMMouseClickStrokeColor = "none",
                     HISTOGRAMMouseClickStrokeWidth = "none", HISTOGRAMMouseDownDisplay = FALSE,
                     HISTOGRAMMouseDownColor = "red", HISTOGRAMMouseDownOpacity = 1,
                     HISTOGRAMMouseDownStrokeColor = "none", HISTOGRAMMouseDownStrokeWidth = "none",
                     HISTOGRAMMouseEnterDisplay = FALSE, HISTOGRAMMouseEnterColor = "red",
                     HISTOGRAMMouseEnterOpacity = 1, HISTOGRAMMouseEnterStrokeColor = "none",
                     HISTOGRAMMouseEnterStrokeWidth = "none", HISTOGRAMMouseLeaveDisplay = FALSE,
                     HISTOGRAMMouseLeaveColor = "red", HISTOGRAMMouseLeaveOpacity = 1,
                     HISTOGRAMMouseLeaveStrokeColor = "none", HISTOGRAMMouseLeaveStrokeWidth = "none",
                     HISTOGRAMMouseMoveDisplay = FALSE, HISTOGRAMMouseMoveColor = "red",
                     HISTOGRAMMouseMoveOpacity = 1, HISTOGRAMMouseMoveStrokeColor = "none",
                     HISTOGRAMMouseMoveStrokeWidth = "none", HISTOGRAMMouseOutDisplay = FALSE,
                     HISTOGRAMMouseOutAnimationTime = 500, HISTOGRAMMouseOutColor = "red",
                     HISTOGRAMMouseOutOpacity = 1, HISTOGRAMMouseOutStrokeColor = "none",
                     HISTOGRAMMouseOutStrokeWidth = "none", HISTOGRAMMouseUpDisplay = FALSE,
                     HISTOGRAMMouseUpColor = "red", HISTOGRAMMouseUpOpacity = 1,
                     HISTOGRAMMouseUpStrokeColor = "none", HISTOGRAMMouseUpStrokeWidth = "none",
                     HISTOGRAMMouseOverDisplay = FALSE, HISTOGRAMMouseOverColor = "red",
                     HISTOGRAMMouseOverOpacity = 1, HISTOGRAMMouseOverStrokeColor = "none",
                     HISTOGRAMMouseOverStrokeWidth = "none", HISTOGRAMMouseOverTooltipsSetting = "style1",
                     HISTOGRAMMouseOverTooltipsHtml = " ", HISTOGRAMMouseOverTooltipsPosition = "absolute",
                     HISTOGRAMMouseOverTooltipsBackgroundColor = "white", HISTOGRAMMouseOverTooltipsBorderStyle = "solid",
                     HISTOGRAMMouseOverTooltipsBorderWidth = 0, HISTOGRAMMouseOverTooltipsPadding = "3px",
                     HISTOGRAMMouseOverTooltipsBorderRadius = "3px", HISTOGRAMMouseOverTooltipsOpacity = 1,
                     LINEMouseEvent = TRUE, LINEMouseClickDisplay = FALSE, LINEMouseClickLineOpacity = 1,
                     LINEMouseClickLineStrokeColor = "none", LINEMouseClickLineStrokeWidth = "none",
                     LINEMouseDownDisplay = FALSE, LINEMouseDownLineOpacity = 1,
                     LINEMouseDownLineStrokeColor = "none", LINEMouseDownLineStrokeWidth = "none",
                     LINEMouseEnterDisplay = FALSE, LINEMouseEnterLineOpacity = 1,
                     LINEMouseEnterLineStrokeColor = "none", LINEMouseEnterLineStrokeWidth = "none",
                     LINEMouseLeaveDisplay = FALSE, LINEMouseLeaveLineOpacity = 1,
                     LINEMouseLeaveLineStrokeColor = "none", LINEMouseLeaveLineStrokeWidth = "none",
                     LINEMouseMoveDisplay = FALSE, LINEMouseMoveLineOpacity = 1,
                     LINEMouseMoveLineStrokeColor = "none", LINEMouseMoveLineStrokeWidth = "none",
                     LINEMouseOutDisplay = FALSE, LINEMouseOutAnimationTime = 500,
                     LINEMouseOutLineOpacity = 1, LINEMouseOutLineStrokeColor = "none",
                     LINEMouseOutLineStrokeWidth = "none", LINEMouseUpDisplay = FALSE,
                     LINEMouseUpLineOpacity = 1, LINEMouseUpLineStrokeColor = "none",
                     LINEMouseUpLineStrokeWidth = "none", LINEMouseOverDisplay = FALSE,
                     LINEMouseOverLineOpacity = 1, LINEMouseOverLineStrokeColor = "none", LINEMouseOverLineStrokeWidth = "none",
                     LINEMouseOverTooltipsSetting = "style1", LINEMouseOverTooltipsHtml = " ",
                     LINEMouseOverTooltipsPosition = "absolute", LINEMouseOverTooltipsBackgroundColor = "white",
                     LINEMouseOverTooltipsBorderStyle = "solid", LINEMouseOverTooltipsBorderWidth = 0,
                     LINEMouseOverTooltipsPadding = "3px", LINEMouseOverTooltipsBorderRadius = "3px",
                     LINEMouseOverTooltipsOpacity = 1,
                     WIGMouseEvent = TRUE, WIGMouseClickDisplay = FALSE, WIGMouseClickLineOpacity = 1,
                     WIGMouseClickLineStrokeColor = "none", WIGMouseClickLineStrokeWidth = "none",
                     WIGMouseClickFillColor = "none", WIGMouseDownDisplay = FALSE,
                     WIGMouseDownLineOpacity = 1, WIGMouseDownLineStrokeColor = "none",
                     WIGMouseDownLineStrokeWidth = "none", WIGMouseDownFillColor = "none",
                     WIGMouseEnterDisplay = FALSE, WIGMouseEnterLineOpacity = 1,
                     WIGMouseEnterLineStrokeColor = "none", WIGMouseEnterLineStrokeWidth = "none",
                     WIGMouseEnterFillColor = "none", WIGMouseLeaveDisplay = FALSE,
                     WIGMouseLeaveLineOpacity = 1, WIGMouseLeaveLineStrokeColor = "none",
                     WIGMouseLeaveLineStrokeWidth = "none", WIGMouseLeaveFillColor = "none",
                     WIGMouseMoveDisplay = FALSE, WIGMouseMoveLineOpacity = 1,
                     WIGMouseMoveLineStrokeColor = "none", WIGMouseMoveLineStrokeWidth = "none",
                     WIGMouseMoveFillColor = "none", WIGMouseOutDisplay = FALSE,
                     WIGMouseOutAnimationTime = 500, WIGMouseOutLineOpacity = 1,
                     WIGMouseOutLineStrokeColor = "none", WIGMouseOutLineStrokeWidth = "none",
                     WIGMouseOutFillColor = "none", WIGMouseUpDisplay = FALSE, WIGMouseUpLineOpacity = 1,
                     WIGMouseUpLineStrokeColor = "none", WIGMouseUpLineStrokeWidth = "none",
                     WIGMouseUpFillColor = "none", WIGMouseOverDisplay = FALSE, WIGMouseOverLineOpacity = 1,
                     WIGMouseOverLineStrokeColor = "none", WIGMouseOverLineStrokeWidth = "none",
                     WIGMouseOverFillColor = "none", WIGMouseOverTooltipsSetting = "style1",
                     WIGMouseOverTooltipsHtml = " ", WIGMouseOverTooltipsPosition = "absolute",
                     WIGMouseOverTooltipsBackgroundColor = "white", WIGMouseOverTooltipsBorderStyle = "solid",
                     WIGMouseOverTooltipsBorderWidth = 0, WIGMouseOverTooltipsPadding = "3px",
                     WIGMouseOverTooltipsBorderRadius = "3px", WIGMouseOverTooltipsOpacity = 1,
                     SCATTERxlink = FALSE, SCATTERMouseEvent = TRUE,SCATTERMouseClickDisplay = FALSE,
                     SCATTERMouseClickColor = "red", SCATTERMouseClickCircleSize = 2,
                     SCATTERMouseClickCircleOpacity = 1, SCATTERMouseClickCircleStrokeColor = "none",
                     SCATTERMouseClickCircleStrokeWidth = "none", SCATTERMouseClickTextFromData = "fourth",
                     SCATTERMouseClickTextOpacity = 1, SCATTERMouseClickTextColor = "red",
                     SCATTERMouseClickTextSize = 8, SCATTERMouseClickTextPostionX = 1,
                     SCATTERMouseClickTextPostionY = 10, SCATTERMouseClickTextDrag = TRUE,
                     SCATTERMouseDownDisplay = FALSE, SCATTERMouseDownColor = "red", SCATTERMouseDownCircleSize = 2,
                     SCATTERMouseDownCircleOpacity = 1, SCATTERMouseDownCircleStrokeColor = "none",
                     SCATTERMouseDownCircleStrokeWidth = "none", SCATTERMouseEnterDisplay = FALSE,
                     SCATTERMouseEnterColor = "red", SCATTERMouseEnterCircleSize = 2,
                     SCATTERMouseEnterCircleOpacity = 1, SCATTERMouseEnterCircleStrokeColor = "none",
                     SCATTERMouseEnterCircleStrokeWidth = "none", SCATTERMouseLeaveDisplay = FALSE,
                     SCATTERMouseLeaveColor = "red", SCATTERMouseLeaveCircleSize = 2,
                     SCATTERMouseLeaveCircleOpacity = 1, SCATTERMouseLeaveCircleStrokeColor = "none",
                     SCATTERMouseLeaveCircleStrokeWidth = "none", SCATTERMouseMoveDisplay = FALSE,
                     SCATTERMouseMoveColor = "red", SCATTERMouseMoveCircleSize = 2,
                     SCATTERMouseMoveCircleOpacity = 1, SCATTERMouseMoveCircleStrokeColor = "none",
                     SCATTERMouseMoveCircleStrokeWidth = "none", SCATTERMouseOutDisplay = FALSE,
                     SCATTERMouseOutAnimationTime = 500, SCATTERMouseOutColor = "red",
                     SCATTERMouseOutCircleSize = 2, SCATTERMouseOutCircleOpacity = 1,
                     SCATTERMouseOutCircleStrokeColor = "none", SCATTERMouseOutCircleStrokeWidth = "none",
                     SCATTERMouseUpDisplay = FALSE, SCATTERMouseUpColor = "red",
                     SCATTERMouseUpCircleSize = 2, SCATTERMouseUpCircleOpacity = 1,
                     SCATTERMouseUpCircleStrokeColor = "none", SCATTERMouseUpCircleStrokeWidth = "none",
                     SCATTERMouseOverDisplay = FALSE, SCATTERMouseOverColor = "red",
                     SCATTERMouseOverCircleSize = 2, SCATTERMouseOverCircleOpacity = 1,
                     SCATTERMouseOverCircleStrokeColor = "none", SCATTERMouseOverCircleStrokeWidth = "none",
                     SCATTERMouseOverTooltipsSetting = "style1", SCATTERMouseOverTooltipsHtml = " ",
                     SCATTERMouseOverTooltipsPosition = "absolute", SCATTERMouseOverTooltipsBackgroundColor = "white",
                     SCATTERMouseOverTooltipsBorderStyle = "solid", SCATTERMouseOverTooltipsBorderWidth = 0,
                     SCATTERMouseOverTooltipsPadding = "3px", SCATTERMouseOverTooltipsBorderRadius = "3px",
                     SCATTERMouseOverTooltipsOpacity = 1,
                     ARCxlink = FALSE, ARCMouseEvent = TRUE, ARCMouseClickDisplay = FALSE,
                     ARCMouseClickColor = "red", ARCMouseClickArcOpacity = 1, ARCMouseClickArcStrokeColor = "none",
                     ARCMouseClickArcStrokeWidth = "none", ARCMouseClickTextFromData = "fourth",
                     ARCMouseClickTextOpacity = 1, ARCMouseClickTextColor = "red", ARCMouseClickTextSize = 8,
                     ARCMouseClickTextPostionX = 1, ARCMouseClickTextPostionY = 10, ARCMouseClickTextDrag = TRUE,
                     ARCMouseDownDisplay = FALSE, ARCMouseDownColor = "red", ARCMouseDownArcOpacity = 1,
                     ARCMouseDownArcStrokeColor = "none", ARCMouseDownArcStrokeWidth = "none",
                     ARCMouseEnterDisplay = FALSE, ARCMouseEnterColor = "red", ARCMouseEnterArcOpacity = 1,
                     ARCMouseEnterArcStrokeColor = "none", ARCMouseEnterArcStrokeWidth = "none",
                     ARCMouseLeaveDisplay = FALSE, ARCMouseLeaveColor = "red", ARCMouseLeaveArcOpacity = 1,
                     ARCMouseLeaveArcStrokeColor = "none", ARCMouseLeaveArcStrokeWidth = "none",
                     ARCMouseMoveDisplay = FALSE, ARCMouseMoveColor = "red", ARCMouseMoveArcOpacity = 1,
                     ARCMouseMoveArcStrokeColor = "none", ARCMouseMoveArcStrokeWidth = "none",
                     ARCMouseOutDisplay = FALSE, ARCMouseOutAnimationTime = 500, ARCMouseOutColor = "red",
                     ARCMouseOutArcOpacity = 1, ARCMouseOutArcStrokeColor = "none",
                     ARCMouseOutArcStrokeWidth = "none", ARCMouseUpDisplay = FALSE,
                     ARCMouseUpColor = "red", ARCMouseUpArcOpacity = 1, ARCMouseUpArcStrokeColor = "none",
                     ARCMouseUpArcStrokeWidth = "none", ARCMouseOverDisplay = FALSE,
                     ARCMouseOverColor = "red", ARCMouseOverArcOpacity = 1, ARCMouseOverArcStrokeColor = "none",
                     ARCMouseOverArcStrokeWidth = "none", ARCMouseOverTooltipsSetting = "style1",
                     ARCMouseOverTooltipsHtml = " ", ARCMouseOverTooltipsPosition = "absolute",
                     ARCMouseOverTooltipsBackgroundColor = "white", ARCMouseOverTooltipsBorderStyle = "solid",
                     ARCMouseOverTooltipsBorderWidth = 0, ARCMouseOverTooltipsPadding = "3px",
                     ARCMouseOverTooltipsBorderRadius = "3px", ARCMouseOverTooltipsOpacity = 1,
                     GENExlink = FALSE, GENEMouseEvent = TRUE, GENEMouseClickDisplay = FALSE,
                     GENEMouseClickColor = "red", GENEMouseClickArcOpacity = 1,
                     GENEMouseClickArcStrokeColor = "none", GENEMouseClickArcStrokeWidth = "none",
                     GENEMouseClickTextFromData = "fourth", GENEMouseClickTextOpacity = 1,
                     GENEMouseClickTextColor = "red", GENEMouseClickTextSize = 8,
                     GENEMouseClickTextPostionX = 1, GENEMouseClickTextPostionY = 10,
                     GENEMouseClickTextDrag = TRUE, GENEMouseDownDisplay = FALSE,
                     GENEMouseDownColor = "red", GENEMouseDownArcOpacity = 1,
                     GENEMouseDownArcStrokeColor = "none", GENEMouseDownArcStrokeWidth = "none",
                     GENEMouseEnterDisplay = FALSE, GENEMouseEnterColor = "red",
                     GENEMouseEnterArcOpacity = 1, GENEMouseEnterArcStrokeColor = "none",
                     GENEMouseEnterArcStrokeWidth = "none", GENEMouseLeaveDisplay = FALSE,
                     GENEMouseLeaveColor = "red", GENEMouseLeaveArcOpacity = 1,
                     GENEMouseLeaveArcStrokeColor = "none", GENEMouseLeaveArcStrokeWidth = "none",
                     GENEMouseMoveDisplay = FALSE, GENEMouseMoveColor = "red",
                     GENEMouseMoveArcOpacity = 1, GENEMouseMoveArcStrokeColor = "none",
                     GENEMouseMoveArcStrokeWidth = "none", GENEMouseOutDisplay = FALSE,
                     GENEMouseOutAnimationTime = 500, GENEMouseOutColor = "red",
                     GENEMouseOutArcOpacity = 1, GENEMouseOutArcStrokeColor = "none",
                     GENEMouseOutArcStrokeWidth = "none", GENEMouseUpDisplay = FALSE,
                     GENEMouseUpColor = "red", GENEMouseUpArcOpacity = 1,
                     GENEMouseUpArcStrokeColor = "none", GENEMouseUpArcStrokeWidth = "none",
                     GENEMouseOverDisplay = FALSE, GENEMouseOverColor = "red",
                     GENEMouseOverArcOpacity = 1, GENEMouseOverArcStrokeColor = "none",
                     GENEMouseOverArcStrokeWidth = "none", GENEMouseOverTooltipsSetting = "style1",
                     GENEMouseOverTooltipsHtml = " ", GENEMouseOverTooltipsPosition = "absolute",
                     GENEMouseOverTooltipsBackgroundColor = "white", GENEMouseOverTooltipsBorderStyle = "solid",
                     GENEMouseOverTooltipsBorderWidth = 0, GENEMouseOverTooltipsPadding = "3px",
                     GENEMouseOverTooltipsBorderRadius = "3px", GENEMouseOverTooltipsOpacity = 1,
                     LOLLIPOPxlink = FALSE, LOLLIPOPMouseEvent = TRUE, LOLLIPOPMouseClickDisplay = FALSE,
                     LOLLIPOPMouseClickColor = "red", LOLLIPOPMouseClickCircleSize = 2,
                     LOLLIPOPMouseClickCircleOpacity = 1, LOLLIPOPMouseClickCircleStrokeColor = "none",
                     LOLLIPOPMouseClickCircleStrokeWidth = "none", LOLLIPOPMouseClickTextFromData = "fourth",
                     LOLLIPOPMouseClickTextOpacity = 1, LOLLIPOPMouseClickTextColor = "red",
                     LOLLIPOPMouseClickTextSize = 8, LOLLIPOPMouseClickTextPostionX = 1,
                     LOLLIPOPMouseClickTextPostionY = 10, LOLLIPOPMouseClickTextDrag = TRUE,
                     LOLLIPOPMouseDownDisplay = FALSE, LOLLIPOPMouseDownColor = "red",
                     LOLLIPOPMouseDownCircleSize = 2, LOLLIPOPMouseDownCircleOpacity = 1,
                     LOLLIPOPMouseDownCircleStrokeColor = "none", LOLLIPOPMouseDownCircleStrokeWidth = "none",
                     LOLLIPOPMouseEnterDisplay = FALSE, LOLLIPOPMouseEnterColor = "red",
                     LOLLIPOPMouseEnterCircleSize = 2, LOLLIPOPMouseEnterCircleOpacity = 1,
                     LOLLIPOPMouseEnterCircleStrokeColor = "none", LOLLIPOPMouseEnterCircleStrokeWidth = "none",
                     LOLLIPOPMouseLeaveDisplay = FALSE, LOLLIPOPMouseLeaveColor = "red",
                     LOLLIPOPMouseLeaveCircleSize = 2, LOLLIPOPMouseLeaveCircleOpacity = 1,
                     LOLLIPOPMouseLeaveCircleStrokeColor = "none", LOLLIPOPMouseLeaveCircleStrokeWidth = "none",
                     LOLLIPOPMouseMoveDisplay = FALSE, LOLLIPOPMouseMoveColor = "red",
                     LOLLIPOPMouseMoveCircleSize = 2, LOLLIPOPMouseMoveCircleOpacity = 1,
                     LOLLIPOPMouseMoveCircleStrokeColor = "none", LOLLIPOPMouseMoveCircleStrokeWidth = "none",
                     LOLLIPOPMouseOutDisplay = FALSE, LOLLIPOPMouseOutAnimationTime = 500,
                     LOLLIPOPMouseOutColor = "red", LOLLIPOPMouseOutCircleSize = 2,
                     LOLLIPOPMouseOutCircleOpacity = 1, LOLLIPOPMouseOutCircleStrokeColor = "none",
                     LOLLIPOPMouseOutCircleStrokeWidth = "none", LOLLIPOPMouseUpDisplay = FALSE,
                     LOLLIPOPMouseUpColor = "red", LOLLIPOPMouseUpCircleSize = 2,
                     LOLLIPOPMouseUpCircleOpacity = 1, LOLLIPOPMouseUpCircleStrokeColor = "none",
                     LOLLIPOPMouseUpCircleStrokeWidth = "none", LOLLIPOPMouseOverDisplay = FALSE,
                     LOLLIPOPMouseOverColor = "red", LOLLIPOPMouseOverCircleSize = 2,
                     LOLLIPOPMouseOverCircleOpacity = 1, LOLLIPOPMouseOverCircleStrokeColor = "none",
                     LOLLIPOPMouseOverCircleStrokeWidth = "none", LOLLIPOPMouseOverTooltipsSetting = "style1",
                     LOLLIPOPMouseOverTooltipsHtml = " ", LOLLIPOPMouseOverTooltipsPosition = "absolute",
                     LOLLIPOPMouseOverTooltipsBackgroundColor = "white", LOLLIPOPMouseOverTooltipsBorderStyle = "solid",
                     LOLLIPOPMouseOverTooltipsBorderWidth = 0, LOLLIPOPMouseOverTooltipsPadding = "3px",
                     LOLLIPOPMouseOverTooltipsBorderRadius = "3px", LOLLIPOPMouseOverTooltipsOpacity = 1,
                      elementId = NULL, ...) {

  # If genome is a string, convert to corresponding chromosome lengths
  if(class(genome) == "character"){
    if(genome == "hg19"){
      genome = list("1" = 249250621, #Hg19
                    "2" = 243199373,
                    "3" = 198022430,
                    "4" = 191154276,
                    "5" = 180915260,
                    "6" = 171115067,
                    "7" = 159138663,
                    "8" = 146364022,
                    "9" = 141213431,
                    "10" = 135534747,
                    "11" = 135006516,
                    "12" = 133851895,
                    "13" = 115169878,
                    "14" = 107349540,
                    "15" = 102531392,
                    "16" = 90354753,
                    "17" = 81195210,
                    "18" = 78077248,
                    "19" = 59128983,
                    "20" = 63025520,
                    "21" = 48129895,
                    "22" = 51304566,
                    "X" = 155270560,
                    "Y" = 59373566)
    }
    else{
      stop("\'genome\' parameter should be either a list of chromosome lengths or \'hg19\'.")
    }
  }

  if(class(genome2) == "character"){
    if(genome2 == "hg19"){
      genome2 = list("1" = 249250621, #Hg19
                    "2" = 243199373,
                    "3" = 198022430,
                    "4" = 191154276,
                    "5" = 180915260,
                    "6" = 171115067,
                    "7" = 159138663,
                    "8" = 146364022,
                    "9" = 141213431,
                    "10" = 135534747,
                    "11" = 135006516,
                    "12" = 133851895,
                    "13" = 115169878,
                    "14" = 107349540,
                    "15" = 102531392,
                    "16" = 90354753,
                    "17" = 81195210,
                    "18" = 78077248,
                    "19" = 59128983,
                    "20" = 63025520,
                    "21" = 48129895,
                    "22" = 51304566,
                    "X" = 155270560,
                    "Y" = 59373566)
    }
    else{
      stop("\'genome\' parameter should be either a list of chromosome lengths or \'hg19\'.")
    }
  }

  # If genomeFillColor is a palette, create corresponding color vector
  genomeFillColor = .NGCircosColorCheck(genomeFillColor, length(genome), "genomeFillColor")
  # forward options using x
  x = list(
    message = message,
    tracklist = tracklist,
    genome = genome,
    genome2 = genome2,
    svgClassName = svgClassName,
    genomeFillColor = genomeFillColor,
    chrPad = chrPad,
    innerRadius = innerRadius,
    outerRadius = outerRadius,
    displayGenomeBorder = displayGenomeBorder,
    genomeBorderColor = genomeBorderColor,
    genomeBorderSize = genomeBorderSize,
    genomeTicksDisplay = genomeTicksDisplay,
    genomeTicksLen = genomeTicksLen,
    genomeTicksColor = genomeTicksColor,
    genomeTicksTextSize = genomeTicksTextSize,
    genomeTicksTextColor = genomeTicksTextColor,
    genomeTicksScale = genomeTicksScale,
    genomeTicksRealLength = genomeTicksRealLength,
    genomeTicksOffset = genomeTicksOffset,
    genomeLabelDisplay = genomeLabelDisplay,
    genomeLabelTextSize = genomeLabelTextSize,
    genomeLabelTextColor = genomeLabelTextColor,
    genomeLabelDx = genomeLabelDx,
    genomeLabelDy = genomeLabelDy,
    compareEvent=compareEvent,
    compareEventGroupGapRate=compareEventGroupGapRate,
    compareEventGroupDistance=compareEventGroupDistance,
    zoom = zoom,
    CNVxlink = CNVxlink,
    CNVMouseEvent = CNVMouseEvent,
    CNVMouseClickDisplay = CNVMouseClickDisplay,
    CNVMouseClickColor = CNVMouseClickColor,
    CNVMouseClickArcOpacity = CNVMouseClickArcOpacity,
    CNVMouseClickArcStrokeColor = CNVMouseClickArcStrokeColor,
    CNVMouseClickArcStrokeWidth = CNVMouseClickArcStrokeWidth,
    CNVMouseClickTextFromData = CNVMouseClickTextFromData,
    CNVMouseClickTextOpacity = CNVMouseClickTextOpacity,
    CNVMouseClickTextColor = CNVMouseClickTextColor,
    CNVMouseClickTextSize = CNVMouseClickTextSize,
    CNVMouseClickTextPostionX = CNVMouseClickTextPostionX,
    CNVMouseClickTextPostionY = CNVMouseClickTextPostionY,
    CNVMouseClickTextDrag = CNVMouseClickTextDrag,
    CNVMouseDownDisplay = CNVMouseDownDisplay,
    CNVMouseDownColor = CNVMouseDownColor,
    CNVMouseDownArcOpacity = CNVMouseDownArcOpacity,
    CNVMouseDownArcStrokeColor = CNVMouseDownArcStrokeColor,
    CNVMouseDownArcStrokeWidth = CNVMouseDownArcStrokeWidth,
    CNVMouseEnterDisplay = CNVMouseEnterDisplay,
    CNVMouseEnterColor = CNVMouseEnterColor,
    CNVMouseEnterArcOpacity = CNVMouseEnterArcOpacity,
    CNVMouseEnterArcStrokeColor = CNVMouseEnterArcStrokeColor,
    CNVMouseEnterArcStrokeWidth = CNVMouseEnterArcStrokeWidth,
    CNVMouseLeaveDisplay = CNVMouseLeaveDisplay,
    CNVMouseLeaveColor = CNVMouseLeaveColor,
    CNVMouseLeaveArcOpacity = CNVMouseLeaveArcOpacity,
    CNVMouseLeaveArcStrokeColor = CNVMouseLeaveArcStrokeColor,
    CNVMouseLeaveArcStrokeWidth = CNVMouseLeaveArcStrokeWidth,
    CNVMouseMoveDisplay = CNVMouseMoveDisplay,
    CNVMouseMoveColor = CNVMouseMoveColor,
    CNVMouseMoveArcOpacity = CNVMouseMoveArcOpacity,
    CNVMouseMoveArcStrokeColor = CNVMouseMoveArcStrokeColor,
    CNVMouseMoveArcStrokeWidth = CNVMouseMoveArcStrokeWidth,
    CNVMouseOutDisplay = CNVMouseOutDisplay,
    CNVMouseOutAnimationTime = CNVMouseOutAnimationTime,
    CNVMouseOutColor = CNVMouseOutColor,
    CNVMouseOutArcOpacity =CNVMouseOutArcOpacity,
    CNVMouseOutArcStrokeColor = CNVMouseOutArcStrokeColor,
    CNVMouseOutArcStrokeWidth = CNVMouseOutArcStrokeWidth,
    CNVMouseUpDisplay = CNVMouseUpDisplay,
    CNVMouseUpColor = CNVMouseUpColor,
    CNVMouseUpArcOpacity = CNVMouseUpArcOpacity,
    CNVMouseUpArcStrokeColor = CNVMouseUpArcStrokeColor,
    CNVMouseUpArcStrokeWidth = CNVMouseUpArcStrokeWidth,
    CNVMouseOverDisplay = CNVMouseOverDisplay,
    CNVMouseOverColor = CNVMouseOverColor,
    CNVMouseOverArcOpacity = CNVMouseOverArcOpacity,
    CNVMouseOverArcStrokeColor = CNVMouseOverArcStrokeColor,
    CNVMouseOverArcStrokeWidth = CNVMouseOverArcStrokeWidth,
    CNVMouseOverTooltipsSetting = CNVMouseOverTooltipsSetting,
    CNVMouseOverTooltipsHtml = CNVMouseOverTooltipsHtml,
    CNVMouseOverTooltipsPosition = CNVMouseOverTooltipsPosition,
    CNVMouseOverTooltipsBackgroundColor = CNVMouseOverTooltipsBackgroundColor,
    CNVMouseOverTooltipsBorderStyle = CNVMouseOverTooltipsBorderStyle,
    CNVMouseOverTooltipsBorderWidth = CNVMouseOverTooltipsBorderWidth,
    CNVMouseOverTooltipsPadding = CNVMouseOverTooltipsPadding,
    CNVMouseOverTooltipsBorderRadius = CNVMouseOverTooltipsBorderRadius,
    CNVMouseOverTooltipsOpacity = CNVMouseOverTooltipsOpacity,
    HEATMAPMouseEvent = HEATMAPMouseEvent,
    HEATMAPMouseClickDisplay = HEATMAPMouseClickDisplay,
    HEATMAPMouseClickColor = HEATMAPMouseClickColor,
    HEATMAPMouseClickOpacity = HEATMAPMouseClickOpacity,
    HEATMAPMouseClickStrokeColor = HEATMAPMouseClickStrokeColor,
    HEATMAPMouseClickStrokeWidth = HEATMAPMouseClickStrokeWidth,
    HEATMAPMouseDownDisplay = HEATMAPMouseDownDisplay,
    HEATMAPMouseDownColor = HEATMAPMouseDownColor,
    HEATMAPMouseDownOpacity = HEATMAPMouseDownOpacity,
    HEATMAPMouseDownStrokeColor = HEATMAPMouseDownStrokeColor,
    HEATMAPMouseDownStrokeWidth = HEATMAPMouseDownStrokeWidth,
    HEATMAPMouseEnterDisplay = HEATMAPMouseEnterDisplay,
    HEATMAPMouseEnterColor = HEATMAPMouseEnterColor,
    HEATMAPMouseEnterOpacity = HEATMAPMouseEnterOpacity,
    HEATMAPMouseEnterStrokeColor = HEATMAPMouseEnterStrokeColor,
    HEATMAPMouseEnterStrokeWidth = HEATMAPMouseEnterStrokeWidth,
    HEATMAPMouseLeaveDisplay = HEATMAPMouseLeaveDisplay,
    HEATMAPMouseLeaveColor = HEATMAPMouseLeaveColor,
    HEATMAPMouseLeaveOpacity = HEATMAPMouseLeaveOpacity,
    HEATMAPMouseLeaveStrokeColor = HEATMAPMouseLeaveStrokeColor,
    HEATMAPMouseLeaveStrokeWidth = HEATMAPMouseLeaveStrokeWidth,
    HEATMAPMouseMoveDisplay = HEATMAPMouseMoveDisplay,
    HEATMAPMouseMoveColor = HEATMAPMouseMoveColor,
    HEATMAPMouseMoveOpacity = HEATMAPMouseMoveOpacity,
    HEATMAPMouseMoveStrokeColor = HEATMAPMouseMoveStrokeColor,
    HEATMAPMouseMoveStrokeWidth = HEATMAPMouseMoveStrokeWidth,
    HEATMAPMouseOutDisplay = HEATMAPMouseOutDisplay,
    HEATMAPMouseOutAnimationTime = HEATMAPMouseOutAnimationTime,
    HEATMAPMouseOutColor = HEATMAPMouseOutColor,
    HEATMAPMouseOutOpacity = HEATMAPMouseOutOpacity,
    HEATMAPMouseOutStrokeColor = HEATMAPMouseOutStrokeColor,
    HEATMAPMouseOutStrokeWidth = HEATMAPMouseOutStrokeWidth,
    HEATMAPMouseUpDisplay = HEATMAPMouseUpDisplay,
    HEATMAPMouseUpColor = HEATMAPMouseUpColor,
    HEATMAPMouseUpOpacity = HEATMAPMouseUpOpacity,
    HEATMAPMouseUpStrokeColor = HEATMAPMouseUpStrokeColor,
    HEATMAPMouseUpStrokeWidth = HEATMAPMouseUpStrokeWidth,
    HEATMAPMouseOverDisplay = HEATMAPMouseOverDisplay,
    HEATMAPMouseOverColor = HEATMAPMouseOverColor,
    HEATMAPMouseOverOpacity = HEATMAPMouseOverOpacity,
    HEATMAPMouseOverStrokeColor = HEATMAPMouseOverStrokeColor,
    HEATMAPMouseOverStrokeWidth = HEATMAPMouseOverStrokeWidth,
    HEATMAPMouseOverTooltipsSetting = HEATMAPMouseOverTooltipsSetting,
    HEATMAPMouseOverTooltipsHtml = HEATMAPMouseOverTooltipsHtml,
    HEATMAPMouseOverTooltipsPosition = HEATMAPMouseOverTooltipsPosition,
    HEATMAPMouseOverTooltipsBackgroundColor = HEATMAPMouseOverTooltipsBackgroundColor,
    HEATMAPMouseOverTooltipsBorderStyle = HEATMAPMouseOverTooltipsBorderStyle,
    HEATMAPMouseOverTooltipsBorderWidth = HEATMAPMouseOverTooltipsBorderWidth,
    HEATMAPMouseOverTooltipsPadding = HEATMAPMouseOverTooltipsPadding,
    HEATMAPMouseOverTooltipsBorderRadius = HEATMAPMouseOverTooltipsBorderRadius,
    HEATMAPMouseOverTooltipsOpacity = HEATMAPMouseOverTooltipsOpacity,
    BUBBLExlink = BUBBLExlink,
    BUBBLEMouseEvent = BUBBLEMouseEvent,
    BUBBLEMouseClickDisplay = BUBBLEMouseClickDisplay,
    BUBBLEMouseClickColor = BUBBLEMouseClickColor,
    BUBBLEMouseClickOpacity = BUBBLEMouseClickOpacity,
    BUBBLEMouseClickStrokeColor = BUBBLEMouseClickStrokeColor,
    BUBBLEMouseClickStrokeWidth = BUBBLEMouseClickStrokeWidth,
    BUBBLEMouseDownDisplay = BUBBLEMouseDownDisplay,
    BUBBLEMouseDownColor = BUBBLEMouseDownColor,
    BUBBLEMouseDownOpacity = BUBBLEMouseDownOpacity,
    BUBBLEMouseDownStrokeColor = BUBBLEMouseDownStrokeColor,
    BUBBLEMouseDownStrokeWidth = BUBBLEMouseDownStrokeWidth,
    BUBBLEMouseEnterDisplay = BUBBLEMouseEnterDisplay,
    BUBBLEMouseEnterColor = BUBBLEMouseEnterColor,
    BUBBLEMouseEnterOpacity = BUBBLEMouseEnterOpacity,
    BUBBLEMouseEnterStrokeColor = BUBBLEMouseEnterStrokeColor,
    BUBBLEMouseEnterStrokeWidth = BUBBLEMouseEnterStrokeWidth,
    BUBBLEMouseLeaveDisplay = BUBBLEMouseLeaveDisplay,
    BUBBLEMouseLeaveColor = BUBBLEMouseLeaveColor,
    BUBBLEMouseLeaveOpacity = BUBBLEMouseLeaveOpacity,
    BUBBLEMouseLeaveStrokeColor = BUBBLEMouseLeaveStrokeColor,
    BUBBLEMouseLeaveStrokeWidth = BUBBLEMouseLeaveStrokeWidth,
    BUBBLEMouseMoveDisplay = BUBBLEMouseMoveDisplay,
    BUBBLEMouseMoveColor = BUBBLEMouseMoveColor,
    BUBBLEMouseMoveOpacity = BUBBLEMouseMoveOpacity,
    BUBBLEMouseMoveStrokeColor = BUBBLEMouseMoveStrokeColor,
    BUBBLEMouseMoveStrokeWidth = BUBBLEMouseMoveStrokeWidth,
    BUBBLEMouseOutDisplay = BUBBLEMouseOutDisplay,
    BUBBLEMouseOutAnimationTime = BUBBLEMouseOutAnimationTime,
    BUBBLEMouseOutColor = BUBBLEMouseOutColor,
    BUBBLEMouseOutOpacity = BUBBLEMouseOutOpacity,
    BUBBLEMouseOutStrokeColor = BUBBLEMouseOutStrokeColor,
    BUBBLEMouseOutStrokeWidth = BUBBLEMouseOutStrokeWidth,
    BUBBLEMouseUpDisplay = BUBBLEMouseUpDisplay,
    BUBBLEMouseUpColor = BUBBLEMouseUpColor,
    BUBBLEMouseUpOpacity = BUBBLEMouseUpOpacity,
    BUBBLEMouseUpStrokeColor = BUBBLEMouseUpStrokeColor,
    BUBBLEMouseUpStrokeWidth = BUBBLEMouseUpStrokeWidth,
    BUBBLEMouseOverDisplay = BUBBLEMouseOverDisplay,
    BUBBLEMouseOverColor = BUBBLEMouseOverColor,
    BUBBLEMouseOverOpacity = BUBBLEMouseOverOpacity,
    BUBBLEMouseOverStrokeColor = BUBBLEMouseOverStrokeColor,
    BUBBLEMouseOverStrokeWidth = BUBBLEMouseOverStrokeWidth,
    BUBBLEMouseOverTooltipsSetting = BUBBLEMouseOverTooltipsSetting,
    BUBBLEMouseOverTooltipsHtml = BUBBLEMouseOverTooltipsHtml,
    BUBBLEMouseOverTooltipsPosition = BUBBLEMouseOverTooltipsPosition,
    BUBBLEMouseOverTooltipsBackgroundColor = BUBBLEMouseOverTooltipsBackgroundColor,
    BUBBLEMouseOverTooltipsBorderStyle = BUBBLEMouseOverTooltipsBorderStyle,
    BUBBLEMouseOverTooltipsBorderWidth = BUBBLEMouseOverTooltipsBorderWidth,
    BUBBLEMouseOverTooltipsPadding = BUBBLEMouseOverTooltipsPadding,
    BUBBLEMouseOverTooltipsBorderRadius = BUBBLEMouseOverTooltipsBorderRadius,
    BUBBLEMouseOverTooltipsOpacity = BUBBLEMouseOverTooltipsOpacity,
    SNPxlink = SNPxlink,
    SNPMouseEvent = SNPMouseEvent,
    SNPMouseCombinationEvent = SNPMouseCombinationEvent,
    SNPMouseCombinationImageDisplay = SNPMouseCombinationImageDisplay,
    SNPMouseCombinationImageTitle = SNPMouseCombinationImageTitle,
    SNPMouseCombinationImageTitleSize = SNPMouseCombinationImageTitleSize,
    SNPMouseCombinationImageTitleWeight = SNPMouseCombinationImageTitleWeight,
    SNPMouseCombinationImageTitleColor = SNPMouseCombinationImageTitleColor,
    SNPMouseCombinationImagePositionX = SNPMouseCombinationImagePositionX,
    SNPMouseCombinationImagePositionY = SNPMouseCombinationImagePositionY,
    SNPMouseCombinationImageHeight = SNPMouseCombinationImageHeight,
    SNPMouseCombinationImageWidth = SNPMouseCombinationImageWidth,
    SNPMouseCombinationGraphDisplay = SNPMouseCombinationGraphDisplay,
    SNPMouseCombinationGraphTitle = SNPMouseCombinationGraphTitle,
    SNPMouseCombinationGraphTitleSize = SNPMouseCombinationGraphTitleSize,
    SNPMouseCombinationGraphTitleWeight = SNPMouseCombinationGraphTitleWeight,
    SNPMouseCombinationGraphTitleColor = SNPMouseCombinationGraphTitleColor,
    SNPMouseCombinationGraphType = SNPMouseCombinationGraphType,
    SNPMouseCombinationGraphPositionX = SNPMouseCombinationGraphPositionX,
    SNPMouseCombinationGraphPositionY = SNPMouseCombinationGraphPositionY,
    SNPMouseCombinationGraphHeight = SNPMouseCombinationGraphHeight,
    SNPMouseCombinationGraphWidth = SNPMouseCombinationGraphWidth,
    SNPMouseCombinationGraphHistogramBarColor = SNPMouseCombinationGraphHistogramBarColor,
    SNPMouseCombinationGraphHistogramPadding = SNPMouseCombinationGraphHistogramPadding,
    SNPMouseCombinationGraphHistogramPositionCorrectX = SNPMouseCombinationGraphHistogramPositionCorrectX,
    SNPMouseCombinationGraphPieAutoColor = SNPMouseCombinationGraphPieAutoColor,
    SNPMouseCombinationGraphPieColor = SNPMouseCombinationGraphPieColor,
    SNPMouseCombinationGraphPieSize = SNPMouseCombinationGraphPieSize,
    SNPMouseCombinationGraphPieStroke = SNPMouseCombinationGraphPieStroke,
    SNPMouseCombinationGraphPieStrokeColor = SNPMouseCombinationGraphPieStrokeColor,
    SNPMouseCombinationGraphPieStrokeWidth = SNPMouseCombinationGraphPieStrokeWidth,
    SNPMouseCombinationGraphPieOpacity = SNPMouseCombinationGraphPieOpacity,
    SNPMouseCombinationGraphLineType = SNPMouseCombinationGraphLineType,
    SNPMouseCombinationGraphLineColor = SNPMouseCombinationGraphLineColor,
    SNPMouseCombinationGraphLineWidth = SNPMouseCombinationGraphLineWidth,
    SNPMouseCombinationGraphLinePoint = SNPMouseCombinationGraphLinePoint,
    SNPMouseCombinationGraphLinePointSize = SNPMouseCombinationGraphLinePointSize,
    SNPMouseCombinationGraphLinePointAutoColor = SNPMouseCombinationGraphLinePointAutoColor,
    SNPMouseCombinationGraphLinePointColor = SNPMouseCombinationGraphLinePointColor,
    SNPMouseCombinationGraphLinePointStroke = SNPMouseCombinationGraphLinePointStroke,
    SNPMouseCombinationGraphLinePointStrokeColor = SNPMouseCombinationGraphLinePointStrokeColor,
    SNPMouseCombinationGraphLinePointStrokeWidth = SNPMouseCombinationGraphLinePointStrokeWidth,
    SNPMouseCombinationGraphLinePointOpacity = SNPMouseCombinationGraphLinePointOpacity,
    SNPMouseCombinationGraphLinePositionCorrectX = SNPMouseCombinationGraphLinePositionCorrectX,
    SNPMouseCombinationTextDisplay = SNPMouseCombinationTextDisplay,
    SNPMouseCombinationTextColor = SNPMouseCombinationTextColor,
    SNPMouseCombinationTextSize = SNPMouseCombinationTextSize,
    SNPMouseCombinationTextWeight = SNPMouseCombinationTextWeight,
    SNPMouseCombinationTextPositionCorrectX = SNPMouseCombinationTextPositionCorrectX,
    SNPMouseCombinationTextPositionCorrectY = SNPMouseCombinationTextPositionCorrectY,
    SNPMouseClickDisplay = SNPMouseClickDisplay,
    SNPMouseClickColor = SNPMouseClickColor,
    SNPMouseClickCircleSize = SNPMouseClickCircleSize,
    SNPMouseClickCircleOpacity = SNPMouseClickCircleOpacity,
    SNPMouseClickCircleStrokeColor = SNPMouseClickCircleStrokeColor,
    SNPMouseClickCircleStrokeWidth = SNPMouseClickCircleStrokeWidth,
    SNPMouseClickTextFromData = SNPMouseClickTextFromData,
    SNPMouseClickTextOpacity = SNPMouseClickTextOpacity,
    SNPMouseClickTextColor = SNPMouseClickTextColor,
    SNPMouseClickTextSize = SNPMouseClickTextSize,
    SNPMouseClickTextPostionX = SNPMouseClickTextPostionX,
    SNPMouseClickTextPostionY = SNPMouseClickTextPostionY,
    SNPMouseClickTextDrag = SNPMouseClickTextDrag,
    SNPMouseDownDisplay = SNPMouseDownDisplay,
    SNPMouseDownColor = SNPMouseDownColor,
    SNPMouseDownCircleSize = SNPMouseDownCircleSize,
    SNPMouseDownCircleOpacity = SNPMouseDownCircleOpacity,
    SNPMouseDownCircleStrokeColor = SNPMouseDownCircleStrokeColor,
    SNPMouseDownCircleStrokeWidth = SNPMouseDownCircleStrokeWidth,
    SNPMouseEnterDisplay = SNPMouseEnterDisplay,
    SNPMouseEnterColor = SNPMouseEnterColor,
    SNPMouseEnterCircleSize = SNPMouseEnterCircleSize,
    SNPMouseEnterCircleOpacity = SNPMouseEnterCircleOpacity,
    SNPMouseEnterCircleStrokeColor = SNPMouseEnterCircleStrokeColor,
    SNPMouseEnterCircleStrokeWidth = SNPMouseEnterCircleStrokeWidth,
    SNPMouseLeaveDisplay = SNPMouseLeaveDisplay,
    SNPMouseLeaveColor = SNPMouseLeaveColor,
    SNPMouseLeaveCircleSize = SNPMouseLeaveCircleSize,
    SNPMouseLeaveCircleOpacity = SNPMouseLeaveCircleOpacity,
    SNPMouseLeaveCircleStrokeColor = SNPMouseLeaveCircleStrokeColor,
    SNPMouseLeaveCircleStrokeWidth = SNPMouseLeaveCircleStrokeWidth,
    SNPMouseMoveDisplay = SNPMouseMoveDisplay,
    SNPMouseMoveColor = SNPMouseMoveColor,
    SNPMouseMoveCircleSize = SNPMouseMoveCircleSize,
    SNPMouseMoveCircleOpacity = SNPMouseMoveCircleOpacity,
    SNPMouseMoveCircleStrokeColor = SNPMouseMoveCircleStrokeColor,
    SNPMouseMoveCircleStrokeWidth = SNPMouseMoveCircleStrokeWidth,
    SNPMouseOutDisplay = SNPMouseOutDisplay,
    SNPMouseOutAnimationTime = SNPMouseOutAnimationTime,
    SNPMouseOutColor = SNPMouseOutColor,
    SNPMouseOutCircleSize = SNPMouseOutCircleSize,
    SNPMouseOutCircleOpacity = SNPMouseOutCircleOpacity,
    SNPMouseOutCircleStrokeColor = SNPMouseOutCircleStrokeColor,
    SNPMouseOutCircleStrokeWidth = SNPMouseOutCircleStrokeWidth,
    SNPMouseUpDisplay = SNPMouseUpDisplay,
    SNPMouseUpColor = SNPMouseUpColor,
    SNPMouseUpCircleSize = SNPMouseUpCircleSize,
    SNPMouseUpCircleOpacity = SNPMouseUpCircleOpacity,
    SNPMouseUpCircleStrokeColor = SNPMouseUpCircleStrokeColor,
    SNPMouseUpCircleStrokeWidth = SNPMouseUpCircleStrokeWidth,
    SNPMouseOverDisplay = SNPMouseOverDisplay,
    SNPMouseOverColor = SNPMouseOverColor,
    SNPMouseOverCircleSize = SNPMouseOverCircleSize,
    SNPMouseOverCircleOpacity = SNPMouseOverCircleOpacity,
    SNPMouseOverCircleStrokeColor = SNPMouseOverCircleStrokeColor,
    SNPMouseOverCircleStrokeWidth = SNPMouseOverCircleStrokeWidth,
    SNPMouseOverTooltipsSetting = SNPMouseOverTooltipsSetting,
    SNPMouseOverTooltipsHtml = SNPMouseOverTooltipsHtml,
    SNPMouseOverTooltipsPosition = SNPMouseOverTooltipsPosition,
    SNPMouseOverTooltipsBackgroundColor = SNPMouseOverTooltipsBackgroundColor,
    SNPMouseOverTooltipsBorderStyle = SNPMouseOverTooltipsBorderStyle,
    SNPMouseOverTooltipsBorderWidth = SNPMouseOverTooltipsBorderWidth,
    SNPMouseOverTooltipsPadding = SNPMouseOverTooltipsPadding,
    SNPMouseOverTooltipsBorderRadius = SNPMouseOverTooltipsBorderRadius,
    SNPMouseOverTooltipsOpacity = SNPMouseOverTooltipsOpacity,
    TEXTModuleDragEvent = TEXTModuleDragEvent,
    LINKxlink = LINKxlink,
    LINKMouseEvent = LINKMouseEvent,
    LINKMouseClickDisplay = LINKMouseClickDisplay,
    LINKMouseClickOpacity = LINKMouseClickOpacity,
    LINKMouseClickStrokeColor = LINKMouseClickStrokeColor,
    LINKMouseClickStrokeWidth = LINKMouseClickStrokeWidth,
    LINKMouseDownDisplay = LINKMouseDownDisplay,
    LINKMouseDownOpacity = LINKMouseDownOpacity,
    LINKMouseDownStrokeColor = LINKMouseDownStrokeColor,
    LINKMouseDownStrokeWidth = LINKMouseDownStrokeWidth,
    LINKMouseEnterDisplay = LINKMouseEnterDisplay,
    LINKMouseEnterOpacity = LINKMouseEnterOpacity,
    LINKMouseEnterStrokeColor = LINKMouseEnterStrokeColor,
    LINKMouseEnterStrokeWidth = LINKMouseEnterStrokeWidth,
    LINKMouseLeaveDisplay = LINKMouseLeaveDisplay,
    LINKMouseLeaveOpacity = LINKMouseLeaveOpacity,
    LINKMouseLeaveStrokeColor = LINKMouseLeaveStrokeColor,
    LINKMouseLeaveStrokeWidth = LINKMouseLeaveStrokeWidth,
    LINKMouseMoveDisplay = LINKMouseMoveDisplay,
    LINKMouseMoveOpacity = LINKMouseMoveOpacity,
    LINKMouseMoveStrokeColor = LINKMouseMoveStrokeColor,
    LINKMouseMoveStrokeWidth = LINKMouseMoveStrokeWidth,
    LINKMouseOutDisplay = LINKMouseOutDisplay,
    LINKMouseOutAnimationTime = LINKMouseOutAnimationTime,
    LINKMouseOutOpacity = LINKMouseOutOpacity,
    LINKMouseOutStrokeColor = LINKMouseOutStrokeColor,
    LINKMouseOutStrokeWidth = LINKMouseOutStrokeWidth,
    LINKMouseUpDisplay = LINKMouseUpDisplay,
    LINKMouseUpOpacity = LINKMouseUpOpacity,
    LINKMouseUpStrokeColor = LINKMouseUpStrokeColor,
    LINKMouseUpStrokeWidth = LINKMouseUpStrokeWidth,
    LINKMouseOverDisplay = LINKMouseOverDisplay,
    LINKMouseOverOpacity = LINKMouseOverOpacity,
    LINKMouseOverStrokeColor = LINKMouseOverStrokeColor,
    LINKMouseOverStrokeWidth = LINKMouseOverStrokeWidth,
    LINKMouseOverTooltipsSetting = LINKMouseOverTooltipsSetting,
    LINKMouseOverTooltipsHtml = LINKMouseOverTooltipsHtml,
    LINKMouseOverTooltipsPosition = LINKMouseOverTooltipsPosition,
    LINKMouseOverTooltipsBackgroundColor = LINKMouseOverTooltipsBackgroundColor,
    LINKMouseOverTooltipsBorderStyle = LINKMouseOverTooltipsBorderStyle,
    LINKMouseOverTooltipsBorderWidth = LINKMouseOverTooltipsBorderWidth,
    LINKMouseOverTooltipsPadding = LINKMouseOverTooltipsPadding,
    LINKMouseOverTooltipsBorderRadius = LINKMouseOverTooltipsBorderRadius,
    LINKMouseOverTooltipsOpacity = LINKMouseOverTooltipsOpacity,
    LINKLabelDragEvent = LINKLabelDragEvent,
    CHORDMouseEvent = CHORDMouseEvent,
    CHORDMouseFillColorExcluded = CHORDMouseFillColorExcluded,
    CHORDMouseClickDisplay = CHORDMouseClickDisplay,
    CHORDMouseClickOpacity = CHORDMouseClickOpacity,
    CHORDMouseClickStrokeColor = CHORDMouseClickStrokeColor,
    CHORDMouseClickStrokeWidth = CHORDMouseClickStrokeWidth,
    CHORDMouseDownDisplay = CHORDMouseDownDisplay,
    CHORDMouseDownOpacity = CHORDMouseDownOpacity,
    CHORDMouseDownStrokeColor = CHORDMouseDownStrokeColor,
    CHORDMouseDownStrokeWidth = CHORDMouseDownStrokeWidth,
    CHORDMouseEnterDisplay = CHORDMouseEnterDisplay,
    CHORDMouseEnterOpacity = CHORDMouseEnterOpacity,
    CHORDMouseEnterStrokeColor = CHORDMouseEnterStrokeColor,
    CHORDMouseEnterStrokeWidth = CHORDMouseEnterStrokeWidth,
    CHORDMouseLeaveDisplay = CHORDMouseLeaveDisplay,
    CHORDMouseLeaveOpacity = CHORDMouseLeaveOpacity,
    CHORDMouseLeaveStrokeColor = CHORDMouseLeaveStrokeColor,
    CHORDMouseLeaveStrokeWidth = CHORDMouseLeaveStrokeWidth,
    CHORDMouseMoveDisplay = CHORDMouseMoveDisplay,
    CHORDMouseMoveOpacity = CHORDMouseMoveOpacity,
    CHORDMouseMoveStrokeColor = CHORDMouseMoveStrokeColor,
    CHORDMouseMoveStrokeWidth = CHORDMouseMoveStrokeWidth,
    CHORDMouseOutDisplay = CHORDMouseOutDisplay,
    CHORDMouseOutAnimationTime = CHORDMouseOutAnimationTime,
    CHORDMouseOutOpacity = CHORDMouseOutOpacity,
    CHORDMouseOutStrokeColor = CHORDMouseOutStrokeColor,
    CHORDMouseOutStrokeWidth = CHORDMouseOutStrokeWidth,
    CHORDMouseUpDisplay = CHORDMouseUpDisplay,
    CHORDMouseUpOpacity = CHORDMouseUpOpacity,
    CHORDMouseUpStrokeColor = CHORDMouseUpStrokeColor,
    CHORDMouseUpStrokeWidth = CHORDMouseUpStrokeWidth,
    CHORDMouseOverDisplay = CHORDMouseOverDisplay,
    CHORDMouseOverOpacity = CHORDMouseOverOpacity,
    CHORDMouseOverStrokeColor = CHORDMouseOverStrokeColor,
    CHORDMouseOverStrokeWidth = CHORDMouseOverStrokeWidth,
    HISTOGRAMxlink = HISTOGRAMxlink,
    HISTOGRAMMouseEvent = HISTOGRAMMouseEvent,
    HISTOGRAMMouseClickDisplay = HISTOGRAMMouseClickDisplay,
    HISTOGRAMMouseClickColor = HISTOGRAMMouseClickColor,
    HISTOGRAMMouseClickOpacity = HISTOGRAMMouseClickOpacity,
    HISTOGRAMMouseClickStrokeColor = HISTOGRAMMouseClickStrokeColor,
    HISTOGRAMMouseClickStrokeWidth = HISTOGRAMMouseClickStrokeWidth,
    HISTOGRAMMouseDownDisplay = HISTOGRAMMouseDownDisplay,
    HISTOGRAMMouseDownColor = HISTOGRAMMouseDownColor,
    HISTOGRAMMouseDownOpacity = HISTOGRAMMouseDownOpacity,
    HISTOGRAMMouseDownStrokeColor = HISTOGRAMMouseDownStrokeColor,
    HISTOGRAMMouseDownStrokeWidth = HISTOGRAMMouseDownStrokeWidth,
    HISTOGRAMMouseEnterDisplay = HISTOGRAMMouseEnterDisplay,
    HISTOGRAMMouseEnterColor = HISTOGRAMMouseEnterColor,
    HISTOGRAMMouseEnterOpacity = HISTOGRAMMouseEnterOpacity,
    HISTOGRAMMouseEnterStrokeColor = HISTOGRAMMouseEnterStrokeColor,
    HISTOGRAMMouseEnterStrokeWidth = HISTOGRAMMouseEnterStrokeWidth,
    HISTOGRAMMouseLeaveDisplay = HISTOGRAMMouseLeaveDisplay,
    HISTOGRAMMouseLeaveColor = HISTOGRAMMouseLeaveColor,
    HISTOGRAMMouseLeaveOpacity = HISTOGRAMMouseLeaveOpacity,
    HISTOGRAMMouseLeaveStrokeColor = HISTOGRAMMouseLeaveStrokeColor,
    HISTOGRAMMouseLeaveStrokeWidth = HISTOGRAMMouseLeaveStrokeWidth,
    HISTOGRAMMouseMoveDisplay = HISTOGRAMMouseMoveDisplay,
    HISTOGRAMMouseMoveColor = HISTOGRAMMouseMoveColor,
    HISTOGRAMMouseMoveOpacity = HISTOGRAMMouseMoveOpacity,
    HISTOGRAMMouseMoveStrokeColor = HISTOGRAMMouseMoveStrokeColor,
    HISTOGRAMMouseMoveStrokeWidth = HISTOGRAMMouseMoveStrokeWidth,
    HISTOGRAMMouseOutDisplay = HISTOGRAMMouseOutDisplay,
    HISTOGRAMMouseOutAnimationTime = HISTOGRAMMouseOutAnimationTime,
    HISTOGRAMMouseOutColor = HISTOGRAMMouseOutColor,
    HISTOGRAMMouseOutOpacity = HISTOGRAMMouseOutOpacity,
    HISTOGRAMMouseOutStrokeColor = HISTOGRAMMouseOutStrokeColor,
    HISTOGRAMMouseOutStrokeWidth = HISTOGRAMMouseOutStrokeWidth,
    HISTOGRAMMouseUpDisplay = HISTOGRAMMouseUpDisplay,
    HISTOGRAMMouseUpColor = HISTOGRAMMouseUpColor,
    HISTOGRAMMouseUpOpacity = HISTOGRAMMouseUpOpacity,
    HISTOGRAMMouseUpStrokeColor = HISTOGRAMMouseUpStrokeColor,
    HISTOGRAMMouseUpStrokeWidth = HISTOGRAMMouseUpStrokeWidth,
    HISTOGRAMMouseOverDisplay = HISTOGRAMMouseOverDisplay,
    HISTOGRAMMouseOverColor = HISTOGRAMMouseOverColor,
    HISTOGRAMMouseOverOpacity = HISTOGRAMMouseOverOpacity,
    HISTOGRAMMouseOverStrokeColor = HISTOGRAMMouseOverStrokeColor,
    HISTOGRAMMouseOverStrokeWidth = HISTOGRAMMouseOverStrokeWidth,
    HISTOGRAMMouseOverTooltipsSetting = HISTOGRAMMouseOverTooltipsSetting,
    HISTOGRAMMouseOverTooltipsHtml = HISTOGRAMMouseOverTooltipsHtml,
    HISTOGRAMMouseOverTooltipsPosition = HISTOGRAMMouseOverTooltipsPosition,
    HISTOGRAMMouseOverTooltipsBackgroundColor = HISTOGRAMMouseOverTooltipsBackgroundColor,
    HISTOGRAMMouseOverTooltipsBorderStyle = HISTOGRAMMouseOverTooltipsBorderStyle,
    HISTOGRAMMouseOverTooltipsBorderWidth = HISTOGRAMMouseOverTooltipsBorderWidth,
    HISTOGRAMMouseOverTooltipsPadding = HISTOGRAMMouseOverTooltipsPadding,
    HISTOGRAMMouseOverTooltipsBorderRadius = HISTOGRAMMouseOverTooltipsBorderRadius,
    HISTOGRAMMouseOverTooltipsOpacity = HISTOGRAMMouseOverTooltipsOpacity,
    LINEMouseEvent = LINEMouseEvent,
    LINEMouseClickDisplay = LINEMouseClickDisplay,
    LINEMouseClickLineOpacity = LINEMouseClickLineOpacity,
    LINEMouseClickLineStrokeColor = LINEMouseClickLineStrokeColor,
    LINEMouseClickLineStrokeWidth = LINEMouseClickLineStrokeWidth,
    LINEMouseDownDisplay = LINEMouseDownDisplay,
    LINEMouseDownLineOpacity = LINEMouseDownLineOpacity,
    LINEMouseDownLineStrokeColor = LINEMouseDownLineStrokeColor,
    LINEMouseDownLineStrokeWidth = LINEMouseDownLineStrokeWidth,
    LINEMouseEnterDisplay = LINEMouseEnterDisplay,
    LINEMouseEnterLineOpacity = LINEMouseEnterLineOpacity,
    LINEMouseEnterLineStrokeColor = LINEMouseEnterLineStrokeColor,
    LINEMouseEnterLineStrokeWidth = LINEMouseEnterLineStrokeWidth,
    LINEMouseLeaveDisplay = LINEMouseLeaveDisplay,
    LINEMouseLeaveLineOpacity = LINEMouseLeaveLineOpacity,
    LINEMouseLeaveLineStrokeColor = LINEMouseLeaveLineStrokeColor,
    LINEMouseLeaveLineStrokeWidth = LINEMouseLeaveLineStrokeWidth,
    LINEMouseMoveDisplay = LINEMouseMoveDisplay,
    LINEMouseMoveLineOpacity = LINEMouseMoveLineOpacity,
    LINEMouseMoveLineStrokeColor = LINEMouseMoveLineStrokeColor,
    LINEMouseMoveLineStrokeWidth = LINEMouseMoveLineStrokeWidth,
    LINEMouseOutDisplay = LINEMouseOutDisplay,
    LINEMouseOutAnimationTime = LINEMouseOutAnimationTime,
    LINEMouseOutLineOpacity = LINEMouseOutLineOpacity,
    LINEMouseOutLineStrokeColor = LINEMouseOutLineStrokeColor,
    LINEMouseOutLineStrokeWidth = LINEMouseOutLineStrokeWidth,
    LINEMouseUpDisplay = LINEMouseUpDisplay,
    LINEMouseUpLineOpacity = LINEMouseUpLineOpacity,
    LINEMouseUpLineStrokeColor = LINEMouseUpLineStrokeColor,
    LINEMouseUpLineStrokeWidth = LINEMouseUpLineStrokeWidth,
    LINEMouseOverDisplay = LINEMouseOverDisplay,
    LINEMouseOverLineOpacity = LINEMouseOverLineOpacity,
    LINEMouseOverLineStrokeColor = LINEMouseOverLineStrokeColor,
    LINEMouseOverLineStrokeWidth = LINEMouseOverLineStrokeWidth,
    LINEMouseOverTooltipsSetting = LINEMouseOverTooltipsSetting,
    LINEMouseOverTooltipsHtml = LINEMouseOverTooltipsHtml,
    LINEMouseOverTooltipsPosition = LINEMouseOverTooltipsPosition,
    LINEMouseOverTooltipsBackgroundColor = LINEMouseOverTooltipsBackgroundColor,
    LINEMouseOverTooltipsBorderStyle = LINEMouseOverTooltipsBorderStyle,
    LINEMouseOverTooltipsBorderWidth = LINEMouseOverTooltipsBorderWidth,
    LINEMouseOverTooltipsPadding = LINEMouseOverTooltipsPadding,
    LINEMouseOverTooltipsBorderRadius = LINEMouseOverTooltipsBorderRadius,
    LINEMouseOverTooltipsOpacity = LINEMouseOverTooltipsOpacity,
    WIGMouseEvent = WIGMouseEvent,
    WIGMouseClickDisplay = WIGMouseClickDisplay,
    WIGMouseClickLineOpacity = WIGMouseClickLineOpacity,
    WIGMouseClickLineStrokeColor = WIGMouseClickLineStrokeColor,
    WIGMouseClickLineStrokeWidth = WIGMouseClickLineStrokeWidth,
    WIGMouseClickFillColor = WIGMouseClickFillColor,
    WIGMouseDownDisplay = WIGMouseDownDisplay,
    WIGMouseDownLineOpacity = WIGMouseDownLineOpacity,
    WIGMouseDownLineStrokeColor = WIGMouseDownLineStrokeColor,
    WIGMouseDownLineStrokeWidth = WIGMouseDownLineStrokeWidth,
    WIGMouseDownFillColor = WIGMouseDownFillColor,
    WIGMouseEnterDisplay = WIGMouseEnterDisplay,
    WIGMouseEnterLineOpacity = WIGMouseEnterLineOpacity,
    WIGMouseEnterLineStrokeColor = WIGMouseEnterLineStrokeColor,
    WIGMouseEnterLineStrokeWidth = WIGMouseEnterLineStrokeWidth,
    WIGMouseEnterFillColor = WIGMouseEnterFillColor,
    WIGMouseLeaveDisplay = WIGMouseLeaveDisplay,
    WIGMouseLeaveLineOpacity = WIGMouseLeaveLineOpacity,
    WIGMouseLeaveLineStrokeColor = WIGMouseLeaveLineStrokeColor,
    WIGMouseLeaveLineStrokeWidth = WIGMouseLeaveLineStrokeWidth,
    WIGMouseLeaveFillColor = WIGMouseLeaveFillColor,
    WIGMouseMoveDisplay = WIGMouseMoveDisplay,
    WIGMouseMoveLineOpacity = WIGMouseMoveLineOpacity,
    WIGMouseMoveLineStrokeColor = WIGMouseMoveLineStrokeColor,
    WIGMouseMoveLineStrokeWidth = WIGMouseMoveLineStrokeWidth,
    WIGMouseMoveFillColor = WIGMouseMoveFillColor,
    WIGMouseOutDisplay = WIGMouseOutDisplay,
    WIGMouseOutAnimationTime = WIGMouseOutAnimationTime,
    WIGMouseOutLineOpacity = WIGMouseOutLineOpacity,
    WIGMouseOutLineStrokeColor = WIGMouseOutLineStrokeColor,
    WIGMouseOutLineStrokeWidth = WIGMouseOutLineStrokeWidth,
    WIGMouseOutFillColor = WIGMouseOutFillColor,
    WIGMouseUpDisplay = WIGMouseUpDisplay,
    WIGMouseUpLineOpacity = WIGMouseUpLineOpacity,
    WIGMouseUpLineStrokeColor = WIGMouseUpLineStrokeColor,
    WIGMouseUpLineStrokeWidth = WIGMouseUpLineStrokeWidth,
    WIGMouseUpFillColor = WIGMouseUpFillColor,
    WIGMouseOverDisplay = WIGMouseOverDisplay,
    WIGMouseOverLineOpacity = WIGMouseOverLineOpacity,
    WIGMouseOverLineStrokeColor = WIGMouseOverLineStrokeColor,
    WIGMouseOverLineStrokeWidth = WIGMouseOverLineStrokeWidth,
    WIGMouseOverFillColor = WIGMouseOverFillColor,
    WIGMouseOverTooltipsSetting = WIGMouseOverTooltipsSetting,
    WIGMouseOverTooltipsHtml = WIGMouseOverTooltipsHtml,
    WIGMouseOverTooltipsPosition = WIGMouseOverTooltipsPosition,
    WIGMouseOverTooltipsBackgroundColor = WIGMouseOverTooltipsBackgroundColor,
    WIGMouseOverTooltipsBorderStyle = WIGMouseOverTooltipsBorderStyle,
    WIGMouseOverTooltipsBorderWidth = WIGMouseOverTooltipsBorderWidth,
    WIGMouseOverTooltipsPadding = WIGMouseOverTooltipsPadding,
    WIGMouseOverTooltipsBorderRadius = WIGMouseOverTooltipsBorderRadius,
    WIGMouseOverTooltipsOpacity = WIGMouseOverTooltipsOpacity,
    SCATTERxlink = SCATTERxlink,
    SCATTERMouseEvent = SCATTERMouseEvent,
    SCATTERMouseClickDisplay = SCATTERMouseClickDisplay,
    SCATTERMouseClickColor = SCATTERMouseClickColor,
    SCATTERMouseClickCircleSize = SCATTERMouseClickCircleSize,
    SCATTERMouseClickCircleOpacity = SCATTERMouseClickCircleOpacity,
    SCATTERMouseClickCircleStrokeColor = SCATTERMouseClickCircleStrokeColor,
    SCATTERMouseClickCircleStrokeWidth = SCATTERMouseClickCircleStrokeWidth,
    SCATTERMouseClickTextFromData = SCATTERMouseClickTextFromData,
    SCATTERMouseClickTextOpacity = SCATTERMouseClickTextOpacity,
    SCATTERMouseClickTextColor = SCATTERMouseClickTextColor,
    SCATTERMouseClickTextSize = SCATTERMouseClickTextSize,
    SCATTERMouseClickTextPostionX = SCATTERMouseClickTextPostionX,
    SCATTERMouseClickTextPostionY = SCATTERMouseClickTextPostionY,
    SCATTERMouseClickTextDrag = SCATTERMouseClickTextDrag,
    SCATTERMouseDownDisplay = SCATTERMouseDownDisplay,
    SCATTERMouseDownColor = SCATTERMouseDownColor,
    SCATTERMouseDownCircleSize = SCATTERMouseDownCircleSize,
    SCATTERMouseDownCircleOpacity = SCATTERMouseDownCircleOpacity,
    SCATTERMouseDownCircleStrokeColor = SCATTERMouseDownCircleStrokeColor,
    SCATTERMouseDownCircleStrokeWidth = SCATTERMouseDownCircleStrokeWidth,
    SCATTERMouseEnterDisplay = SCATTERMouseEnterDisplay,
    SCATTERMouseEnterColor = SCATTERMouseEnterColor,
    SCATTERMouseEnterCircleSize = SCATTERMouseEnterCircleSize,
    SCATTERMouseEnterCircleOpacity = SCATTERMouseEnterCircleOpacity,
    SCATTERMouseEnterCircleStrokeColor = SCATTERMouseEnterCircleStrokeColor,
    SCATTERMouseEnterCircleStrokeWidth = SCATTERMouseEnterCircleStrokeWidth,
    SCATTERMouseLeaveDisplay = SCATTERMouseLeaveDisplay,
    SCATTERMouseLeaveColor = SCATTERMouseLeaveColor,
    SCATTERMouseLeaveCircleSize = SCATTERMouseLeaveCircleSize,
    SCATTERMouseLeaveCircleOpacity = SCATTERMouseLeaveCircleOpacity,
    SCATTERMouseLeaveCircleStrokeColor = SCATTERMouseLeaveCircleStrokeColor,
    SCATTERMouseLeaveCircleStrokeWidth = SCATTERMouseLeaveCircleStrokeWidth,
    SCATTERMouseMoveDisplay = SCATTERMouseMoveDisplay,
    SCATTERMouseMoveColor = SCATTERMouseMoveColor,
    SCATTERMouseMoveCircleSize = SCATTERMouseMoveCircleSize,
    SCATTERMouseMoveCircleOpacity = SCATTERMouseMoveCircleOpacity,
    SCATTERMouseMoveCircleStrokeColor = SCATTERMouseMoveCircleStrokeColor,
    SCATTERMouseMoveCircleStrokeWidth = SCATTERMouseMoveCircleStrokeWidth,
    SCATTERMouseOutDisplay = SCATTERMouseOutDisplay,
    SCATTERMouseOutAnimationTime = SCATTERMouseOutAnimationTime,
    SCATTERMouseOutColor = SCATTERMouseOutColor,
    SCATTERMouseOutCircleSize = SCATTERMouseOutCircleSize,
    SCATTERMouseOutCircleOpacity = SCATTERMouseOutCircleOpacity,
    SCATTERMouseOutCircleStrokeColor = SCATTERMouseOutCircleStrokeColor,
    SCATTERMouseOutCircleStrokeWidth = SCATTERMouseOutCircleStrokeWidth,
    SCATTERMouseUpDisplay = SCATTERMouseUpDisplay,
    SCATTERMouseUpColor = SCATTERMouseUpColor,
    SCATTERMouseUpCircleSize = SCATTERMouseUpCircleSize,
    SCATTERMouseUpCircleOpacity = SCATTERMouseUpCircleOpacity,
    SCATTERMouseUpCircleStrokeColor = SCATTERMouseUpCircleStrokeColor,
    SCATTERMouseUpCircleStrokeWidth = SCATTERMouseUpCircleStrokeWidth,
    SCATTERMouseOverDisplay = SCATTERMouseOverDisplay,
    SCATTERMouseOverColor = SCATTERMouseOverColor,
    SCATTERMouseOverCircleSize = SCATTERMouseOverCircleSize,
    SCATTERMouseOverCircleOpacity = SCATTERMouseOverCircleOpacity,
    SCATTERMouseOverCircleStrokeColor = SCATTERMouseOverCircleStrokeColor,
    SCATTERMouseOverCircleStrokeWidth = SCATTERMouseOverCircleStrokeWidth,
    SCATTERMouseOverTooltipsSetting = SCATTERMouseOverTooltipsSetting,
    SCATTERMouseOverTooltipsHtml = SCATTERMouseOverTooltipsHtml,
    SCATTERMouseOverTooltipsPosition = SCATTERMouseOverTooltipsPosition,
    SCATTERMouseOverTooltipsBackgroundColor = SCATTERMouseOverTooltipsBackgroundColor,
    SCATTERMouseOverTooltipsBorderStyle = SCATTERMouseOverTooltipsBorderStyle,
    SCATTERMouseOverTooltipsBorderWidth = SCATTERMouseOverTooltipsBorderWidth,
    SCATTERMouseOverTooltipsPadding = SCATTERMouseOverTooltipsPadding,
    SCATTERMouseOverTooltipsBorderRadius = SCATTERMouseOverTooltipsBorderRadius,
    SCATTERMouseOverTooltipsOpacity = SCATTERMouseOverTooltipsOpacity,
    ARCxlink = ARCxlink,
    ARCMouseEvent = ARCMouseEvent,
    ARCMouseClickDisplay = ARCMouseClickDisplay,
    ARCMouseClickColor = ARCMouseClickColor,
    ARCMouseClickArcOpacity = ARCMouseClickArcOpacity,
    ARCMouseClickArcStrokeColor = ARCMouseClickArcStrokeColor,
    ARCMouseClickArcStrokeWidth = ARCMouseClickArcStrokeWidth,
    ARCMouseClickTextFromData = ARCMouseClickTextFromData,
    ARCMouseClickTextOpacity = ARCMouseClickTextOpacity,
    ARCMouseClickTextColor = ARCMouseClickTextColor,
    ARCMouseClickTextSize = ARCMouseClickTextSize,
    ARCMouseClickTextPostionX = ARCMouseClickTextPostionX,
    ARCMouseClickTextPostionY = ARCMouseClickTextPostionY,
    ARCMouseClickTextDrag = ARCMouseClickTextDrag,
    ARCMouseDownDisplay = ARCMouseDownDisplay,
    ARCMouseDownColor = ARCMouseDownColor,
    ARCMouseDownArcOpacity = ARCMouseDownArcOpacity,
    ARCMouseDownArcStrokeColor = ARCMouseDownArcStrokeColor,
    ARCMouseDownArcStrokeWidth = ARCMouseDownArcStrokeWidth,
    ARCMouseEnterDisplay = ARCMouseEnterDisplay,
    ARCMouseEnterColor = ARCMouseEnterColor,
    ARCMouseEnterArcOpacity = ARCMouseEnterArcOpacity,
    ARCMouseEnterArcStrokeColor = ARCMouseEnterArcStrokeColor,
    ARCMouseEnterArcStrokeWidth = ARCMouseEnterArcStrokeWidth,
    ARCMouseLeaveDisplay = ARCMouseLeaveDisplay,
    ARCMouseLeaveColor = ARCMouseLeaveColor,
    ARCMouseLeaveArcOpacity = ARCMouseLeaveArcOpacity,
    ARCMouseLeaveArcStrokeColor = ARCMouseLeaveArcStrokeColor,
    ARCMouseLeaveArcStrokeWidth = ARCMouseLeaveArcStrokeWidth,
    ARCMouseMoveDisplay = ARCMouseMoveDisplay,
    ARCMouseMoveColor = ARCMouseMoveColor,
    ARCMouseMoveArcOpacity = ARCMouseMoveArcOpacity,
    ARCMouseMoveArcStrokeColor = ARCMouseMoveArcStrokeColor,
    ARCMouseMoveArcStrokeWidth = ARCMouseMoveArcStrokeWidth,
    ARCMouseOutDisplay = ARCMouseOutDisplay,
    ARCMouseOutAnimationTime = ARCMouseOutAnimationTime,
    ARCMouseOutColor = ARCMouseOutColor,
    ARCMouseOutArcOpacity = ARCMouseOutArcOpacity,
    ARCMouseOutArcStrokeColor = ARCMouseOutArcStrokeColor,
    ARCMouseOutArcStrokeWidth = ARCMouseOutArcStrokeWidth,
    ARCMouseUpDisplay = ARCMouseUpDisplay,
    ARCMouseUpColor = ARCMouseUpColor,
    ARCMouseUpArcOpacity = ARCMouseUpArcOpacity,
    ARCMouseUpArcStrokeColor = ARCMouseUpArcStrokeColor,
    ARCMouseUpArcStrokeWidth = ARCMouseUpArcStrokeWidth,
    ARCMouseOverDisplay = ARCMouseOverDisplay,
    ARCMouseOverColor = ARCMouseOverColor,
    ARCMouseOverArcOpacity = ARCMouseOverArcOpacity,
    ARCMouseOverArcStrokeColor = ARCMouseOverArcStrokeColor,
    ARCMouseOverArcStrokeWidth = ARCMouseOverArcStrokeWidth,
    ARCMouseOverTooltipsSetting = ARCMouseOverTooltipsSetting,
    ARCMouseOverTooltipsHtml = ARCMouseOverTooltipsHtml,
    ARCMouseOverTooltipsPosition = ARCMouseOverTooltipsPosition,
    ARCMouseOverTooltipsBackgroundColor = ARCMouseOverTooltipsBackgroundColor,
    ARCMouseOverTooltipsBorderStyle = ARCMouseOverTooltipsBorderStyle,
    ARCMouseOverTooltipsBorderWidth = ARCMouseOverTooltipsBorderWidth,
    ARCMouseOverTooltipsPadding = ARCMouseOverTooltipsPadding,
    ARCMouseOverTooltipsBorderRadius = ARCMouseOverTooltipsBorderRadius,
    ARCMouseOverTooltipsOpacity = ARCMouseOverTooltipsOpacity,
    GENExlink = GENExlink,
    GENEMouseEvent = GENEMouseEvent,
    GENEMouseClickDisplay = GENEMouseClickDisplay,
    GENEMouseClickColor = GENEMouseClickColor,
    GENEMouseClickArcOpacity = GENEMouseClickArcOpacity,
    GENEMouseClickArcStrokeColor = GENEMouseClickArcStrokeColor,
    GENEMouseClickArcStrokeWidth = GENEMouseClickArcStrokeWidth,
    GENEMouseClickTextFromData = GENEMouseClickTextFromData,
    GENEMouseClickTextOpacity = GENEMouseClickTextOpacity,
    GENEMouseClickTextColor = GENEMouseClickTextColor,
    GENEMouseClickTextSize = GENEMouseClickTextSize,
    GENEMouseClickTextPostionX = GENEMouseClickTextPostionX,
    GENEMouseClickTextPostionY = GENEMouseClickTextPostionY,
    GENEMouseClickTextDrag = GENEMouseClickTextDrag,
    GENEMouseDownDisplay = GENEMouseDownDisplay,
    GENEMouseDownColor = GENEMouseDownColor,
    GENEMouseDownArcOpacity = GENEMouseDownArcOpacity,
    GENEMouseDownArcStrokeColor = GENEMouseDownArcStrokeColor,
    GENEMouseDownArcStrokeWidth = GENEMouseDownArcStrokeWidth,
    GENEMouseEnterDisplay = GENEMouseEnterDisplay,
    GENEMouseEnterColor = GENEMouseEnterColor,
    GENEMouseEnterArcOpacity = GENEMouseEnterArcOpacity,
    GENEMouseEnterArcStrokeColor = GENEMouseEnterArcStrokeColor,
    GENEMouseEnterArcStrokeWidth = GENEMouseEnterArcStrokeWidth,
    GENEMouseLeaveDisplay = GENEMouseLeaveDisplay,
    GENEMouseLeaveColor = GENEMouseLeaveColor,
    GENEMouseLeaveArcOpacity = GENEMouseLeaveArcOpacity,
    GENEMouseLeaveArcStrokeColor = GENEMouseLeaveArcStrokeColor,
    GENEMouseLeaveArcStrokeWidth = GENEMouseLeaveArcStrokeWidth,
    GENEMouseMoveDisplay = GENEMouseMoveDisplay,
    GENEMouseMoveColor = GENEMouseMoveColor,
    GENEMouseMoveArcOpacity = GENEMouseMoveArcOpacity,
    GENEMouseMoveArcStrokeColor = GENEMouseMoveArcStrokeColor,
    GENEMouseMoveArcStrokeWidth = GENEMouseMoveArcStrokeWidth,
    GENEMouseOutDisplay = GENEMouseOutDisplay,
    GENEMouseOutAnimationTime = GENEMouseOutAnimationTime,
    GENEMouseOutColor = GENEMouseOutColor,
    GENEMouseOutArcOpacity = GENEMouseOutArcOpacity,
    GENEMouseOutArcStrokeColor = GENEMouseOutArcStrokeColor,
    GENEMouseOutArcStrokeWidth = GENEMouseOutArcStrokeWidth,
    GENEMouseUpDisplay = GENEMouseUpDisplay,
    GENEMouseUpColor = GENEMouseUpColor,
    GENEMouseUpArcOpacity = GENEMouseUpArcOpacity,
    GENEMouseUpArcStrokeColor = GENEMouseUpArcStrokeColor,
    GENEMouseUpArcStrokeWidth = GENEMouseUpArcStrokeWidth,
    GENEMouseOverDisplay = GENEMouseOverDisplay,
    GENEMouseOverColor = GENEMouseOverColor,
    GENEMouseOverArcOpacity = GENEMouseOverArcOpacity,
    GENEMouseOverArcStrokeColor = GENEMouseOverArcStrokeColor,
    GENEMouseOverArcStrokeWidth = GENEMouseOverArcStrokeWidth,
    GENEMouseOverTooltipsSetting = GENEMouseOverTooltipsSetting,
    GENEMouseOverTooltipsHtml = GENEMouseOverTooltipsHtml,
    GENEMouseOverTooltipsPosition = GENEMouseOverTooltipsPosition,
    GENEMouseOverTooltipsBackgroundColor = GENEMouseOverTooltipsBackgroundColor,
    GENEMouseOverTooltipsBorderStyle = GENEMouseOverTooltipsBorderStyle,
    GENEMouseOverTooltipsBorderWidth = GENEMouseOverTooltipsBorderWidth,
    GENEMouseOverTooltipsPadding = GENEMouseOverTooltipsPadding,
    GENEMouseOverTooltipsBorderRadius = GENEMouseOverTooltipsBorderRadius,
    GENEMouseOverTooltipsOpacity = GENEMouseOverTooltipsOpacity,
    LOLLIPOPxlink = LOLLIPOPxlink,
    LOLLIPOPMouseEvent = LOLLIPOPMouseEvent,
    LOLLIPOPMouseClickDisplay = LOLLIPOPMouseClickDisplay,
    LOLLIPOPMouseClickColor = LOLLIPOPMouseClickColor,
    LOLLIPOPMouseClickCircleSize = LOLLIPOPMouseClickCircleSize,
    LOLLIPOPMouseClickCircleOpacity = LOLLIPOPMouseClickCircleOpacity,
    LOLLIPOPMouseClickCircleStrokeColor = LOLLIPOPMouseClickCircleStrokeColor,
    LOLLIPOPMouseClickCircleStrokeWidth = LOLLIPOPMouseClickCircleStrokeWidth,
    LOLLIPOPMouseClickTextFromData = LOLLIPOPMouseClickTextFromData,
    LOLLIPOPMouseClickTextOpacity = LOLLIPOPMouseClickTextOpacity,
    LOLLIPOPMouseClickTextColor = LOLLIPOPMouseClickTextColor,
    LOLLIPOPMouseClickTextSize = LOLLIPOPMouseClickTextSize,
    LOLLIPOPMouseClickTextPostionX = LOLLIPOPMouseClickTextPostionX,
    LOLLIPOPMouseClickTextPostionY = LOLLIPOPMouseClickTextPostionY,
    LOLLIPOPMouseClickTextDrag = LOLLIPOPMouseClickTextDrag,
    LOLLIPOPMouseDownDisplay = LOLLIPOPMouseDownDisplay,
    LOLLIPOPMouseDownColor = LOLLIPOPMouseDownColor,
    LOLLIPOPMouseDownCircleSize = LOLLIPOPMouseDownCircleSize,
    LOLLIPOPMouseDownCircleOpacity = LOLLIPOPMouseDownCircleOpacity,
    LOLLIPOPMouseDownCircleStrokeColor = LOLLIPOPMouseDownCircleStrokeColor,
    LOLLIPOPMouseDownCircleStrokeWidth = LOLLIPOPMouseDownCircleStrokeWidth,
    LOLLIPOPMouseEnterDisplay = LOLLIPOPMouseEnterDisplay,
    LOLLIPOPMouseEnterColor = LOLLIPOPMouseEnterColor,
    LOLLIPOPMouseEnterCircleSize = LOLLIPOPMouseEnterCircleSize,
    LOLLIPOPMouseEnterCircleOpacity = LOLLIPOPMouseEnterCircleOpacity,
    LOLLIPOPMouseEnterCircleStrokeColor = LOLLIPOPMouseEnterCircleStrokeColor,
    LOLLIPOPMouseEnterCircleStrokeWidth = LOLLIPOPMouseEnterCircleStrokeWidth,
    LOLLIPOPMouseLeaveDisplay = LOLLIPOPMouseLeaveDisplay,
    LOLLIPOPMouseLeaveColor = LOLLIPOPMouseLeaveColor,
    LOLLIPOPMouseLeaveCircleSize = LOLLIPOPMouseLeaveCircleSize,
    LOLLIPOPMouseLeaveCircleOpacity = LOLLIPOPMouseLeaveCircleOpacity,
    LOLLIPOPMouseLeaveCircleStrokeColor = LOLLIPOPMouseLeaveCircleStrokeColor,
    LOLLIPOPMouseLeaveCircleStrokeWidth = LOLLIPOPMouseLeaveCircleStrokeWidth,
    LOLLIPOPMouseMoveDisplay = LOLLIPOPMouseMoveDisplay,
    LOLLIPOPMouseMoveColor = LOLLIPOPMouseMoveColor,
    LOLLIPOPMouseMoveCircleSize = LOLLIPOPMouseMoveCircleSize,
    LOLLIPOPMouseMoveCircleOpacity = LOLLIPOPMouseMoveCircleOpacity,
    LOLLIPOPMouseMoveCircleStrokeColor = LOLLIPOPMouseMoveCircleStrokeColor,
    LOLLIPOPMouseMoveCircleStrokeWidth = LOLLIPOPMouseMoveCircleStrokeWidth,
    LOLLIPOPMouseOutDisplay = LOLLIPOPMouseOutDisplay,
    LOLLIPOPMouseOutAnimationTime = LOLLIPOPMouseOutAnimationTime,
    LOLLIPOPMouseOutColor = LOLLIPOPMouseOutColor,
    LOLLIPOPMouseOutCircleSize = LOLLIPOPMouseOutCircleSize,
    LOLLIPOPMouseOutCircleOpacity = LOLLIPOPMouseOutCircleOpacity,
    LOLLIPOPMouseOutCircleStrokeColor = LOLLIPOPMouseOutCircleStrokeColor,
    LOLLIPOPMouseOutCircleStrokeWidth = LOLLIPOPMouseOutCircleStrokeWidth,
    LOLLIPOPMouseUpDisplay = LOLLIPOPMouseUpDisplay,
    LOLLIPOPMouseUpColor = LOLLIPOPMouseUpColor,
    LOLLIPOPMouseUpCircleSize = LOLLIPOPMouseUpCircleSize,
    LOLLIPOPMouseUpCircleOpacity = LOLLIPOPMouseUpCircleOpacity,
    LOLLIPOPMouseUpCircleStrokeColor = LOLLIPOPMouseUpCircleStrokeColor,
    LOLLIPOPMouseUpCircleStrokeWidth = LOLLIPOPMouseUpCircleStrokeWidth,
    LOLLIPOPMouseOverDisplay = LOLLIPOPMouseOverDisplay,
    LOLLIPOPMouseOverColor = LOLLIPOPMouseOverColor,
    LOLLIPOPMouseOverCircleSize = LOLLIPOPMouseOverCircleSize,
    LOLLIPOPMouseOverCircleOpacity = LOLLIPOPMouseOverCircleOpacity,
    LOLLIPOPMouseOverCircleStrokeColor = LOLLIPOPMouseOverCircleStrokeColor,
    LOLLIPOPMouseOverCircleStrokeWidth = LOLLIPOPMouseOverCircleStrokeWidth,
    LOLLIPOPMouseOverTooltipsSetting = LOLLIPOPMouseOverTooltipsSetting,
    LOLLIPOPMouseOverTooltipsHtml = LOLLIPOPMouseOverTooltipsHtml,
    LOLLIPOPMouseOverTooltipsPosition = LOLLIPOPMouseOverTooltipsPosition,
    LOLLIPOPMouseOverTooltipsBackgroundColor = LOLLIPOPMouseOverTooltipsBackgroundColor,
    LOLLIPOPMouseOverTooltipsBorderStyle = LOLLIPOPMouseOverTooltipsBorderStyle,
    LOLLIPOPMouseOverTooltipsBorderWidth = LOLLIPOPMouseOverTooltipsBorderWidth,
    LOLLIPOPMouseOverTooltipsPadding = LOLLIPOPMouseOverTooltipsPadding,
    LOLLIPOPMouseOverTooltipsBorderRadius = LOLLIPOPMouseOverTooltipsBorderRadius,
    LOLLIPOPMouseOverTooltipsOpacity = LOLLIPOPMouseOverTooltipsOpacity)
  # create widget
  htmlwidgets::createWidget(
    name = 'NGCircos',
    x,
    width = width,
    height = height,
    package = 'NGCircos',
    elementId = elementId
  )
}

#' Shiny bindings for NGCircos
#'
#' Output and render functions for using NGCircos within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a NGCircos
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name NGCircos-shiny
#'
#' @export
NGCircosOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'NGCircos', width, height, package = 'NGCircos')
}

#' @rdname NGCircos-shiny
#' @export
renderNGCircos <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, NGCircosOutput, env, quoted = TRUE)
}

#' Create a BACKGROUND track to be added to a NGCircos tracklist
#'
#' Simple background to display behind another track
#'
#' @param trackname The name of the new track.
#' @param compareGroup The group number of thic track in compare module
#' @param fillColors The color of the background element, in hexadecimal RGB format.
#' @param borderColors The color of the background borders, in hexadecimal RGB format.
#' @param axisShow Whether show a axis or not
#' @param axisWidth,axisColor,axisOpacity,axisNum The color, opacity value and number of line for axis
#' @param minRadius,maxRadius Where the track should begin and end
#' @param borderSize The thickness of the background borders.
#' @param animationDisplay Whether display a animation or not
#' @param animationTime,animationDelay,animationType The time, delay and display type for animation
#'
#' @param ... Ignored
#'
#' @examples
#' NGCircos(NGCircosBackgroundTrack('bgTrack01', fillColors="#FFEEEE", borderSize = 1))
#'
#' @export
NGCircosBackgroundTrack <- function(trackname,compareGroup = 1, fillColors = "#EEEEFF", borderColors = "#000000",
                                   axisShow = FALSE, axisColor = "#000", axisOpacity = 0.5, axisNum = 4, axisWidth = 0.3,
                                   maxRadius = 190, minRadius = 105, borderSize = 0.3,
                                   animationDisplay = FALSE, animationTime = 2000, animationDelay=20,
                                   animationType = "bounce",...){
  track1 = paste("BACKGROUND", trackname, sep="_")
  track2 = list(compareGroup = compareGroup,axisShow = axisShow, axisWidth = axisWidth, axisColor = axisColor,
                axisOpacity = axisOpacity, axisNum = axisNum, BACKGROUNDAnimationDisplay = animationDisplay,
                BACKGROUNDAnimationTime = animationTime, BACKGROUNDAnimationDelay = animationDelay,
                BACKGROUNDAnimationType = animationType, BgouterRadius = maxRadius, BginnerRadius = minRadius,
                BgFillColor = fillColors, BgborderColor = borderColors, BgborderSize = borderSize)
  track = NGCircosTracklist() + list(list(track1, track2))
  return(track)
}

#' @title Create Text module to be added to a NGCircos tracklist
#'
#' @description Simple text annotation displayed in the visualization
#'
#' @param trackname The name of the new track.
#'
#' @param text The text to be displayed.
#'
#' @param x,y Coordinates of the lower left corner of the annotation
#' @param size Font size, with units specified (such as em or px).
#' @param color Font color, in hexadecimal RGB format.
#' @param weight Font weight. Can be "normal", "bold", "bolder" or "lighter".
#' @param opacity Font opacity.
#' @param rotateRate ratate rate for text
#' @param animationDisplay Whether display a animation or not
#' @param animationInitialSize Initial text size in animation
#' @param animationInitialWeight Initial text weight in animation
#' @param animationInitialColor Initial text color in animation
#' @param animationInitialOpacity Initial text opacity in animation
#' @param animationInitialPositionX,animationInitialPositionY Initial text coordinates in animation(The parameter x,y will become the final position for text if animation displayed)
#' @param animationInitialRotate Initial rotate rate in animation
#' @param animationTime,animationDelay,animationType The time, delay and display type for animation
#'
#' @param ... Ignored
#'
#' @examples
#' NGCircos(NGCircosTextTrack('textTrack01', 'Annotation', color = '#DD2222', x = -40))
#'
#' @export
NGCircosTextTrack <- function(trackname, text,
                             x = 0, y = 0, size = "1.2em", weight = "bold", opacity = 1, color = "#000000",
                             rotateRate = 0, animationDisplay = FALSE, animationInitialSize = 20,
                             animationInitialWeight = "bold", animationInitialColor = "black",
                             animationInitialOpacity = 1, animationInitialPositionX = 0,
                             animationInitialPositionY = 0 , animationInitialRotate = 0,
                             animationDelay = 50, animationTime = 1000, animationType = "linear", ...){
  track1 = paste("TEXT", trackname, sep="_")
  track2 = list(x = x, y = y, textSize = size,textWeight = weight, textColor = color, textOpacity = opacity,
                text = text, rotateRate = rotateRate, TEXTAnimationDisplay = animationDisplay,
                TEXTAnimationInitialSize = animationInitialSize, TEXTAnimationInitialWeight = animationInitialWeight,
                TEXTAnimationInitialColor = animationInitialColor, TEXTAnimationInitialOpacity = animationInitialOpacity,
                TEXTAnimationInitialPositionX = animationInitialPositionX, TEXTAnimationInitialPositionY = animationInitialPositionY,
                TEXTAnimationInitialRotate = animationInitialRotate, TEXTAnimationDelay = animationDelay,
                TEXTAnimationTime = animationTime, TEXTAnimationType = animationType)
  track = NGCircosTracklist() + list(list(track1, track2))
  return(track)
}

#' @title Create a LEGEND module to a NGCircos tracklist
#'
#' @description Simple legend annotation displayed in the visualization.
#'
#' @param trackname The name of the new track.
#'
#' @param x,y Coordinates of the lower left corner of the annotation
#' @param title The title for legend
#' @param size Font size for title, with units specified (such as em or px).
#' @param weight Font weight for title. Can be "normal", "bold", "bolder" or "lighter".
#' @param GapBetweenGraphicText Gap between icon and text in legend.
#' @param GapBetweenLines Gap between each two lines in legend
#' @param data A list of legend with details including type, color, opacity, circleSize, rectSize, lineWidth,
#'             lineHeight, text, textSize and textWeight. Details can be found on NGCircos document.
#'
#' @param ... Ignored
#'
#' @examples
#' legend1 <- list(type= "circle", color="#1E77B4",opacity="1.0",circleSize="8",text= "C.CK", textSize= "14",textWeight="normal")
#' legend2 <- list(type= "circle", color="#AEC7E8",opacity="1.0",circleSize="8",text= "C.NPK", textSize= "14",textWeight="normal")
#' NGCircos(NGCircosLegendTrack('legendTrack01', title = "legend",data=list(legend1,legend2),size = 20))
#'
#' @export
NGCircosLegendTrack <- function(trackname, x = 20, y = 20, title = "legend", size = 6, weight = "normal",
                               GapBetweenGraphicText = 5, GapBetweenLines = 20, data, ...){
  track1 = paste("LEGEND", trackname, sep="_")
  track2 = list(x = x, y = y, title = title, titleSize = size, titleWeight = weight,
                GapBetweenGraphicText = GapBetweenGraphicText, GapBetweenLines = GapBetweenLines)
  track3 = data
  track = NGCircosTracklist() + list(list(track1, track2, track3))
  return(track)
}

#' Create a AUXILIAYLINE module to a NGCircos tracklist
#'
#' A auxiliary line displayed in the visualization
#' The document for more parameters in NGCircos is available at www.xxxxx.com
#'
#' @param trackname The name of the new track.
#'
#' @param startX,startY Start coordinates for auxiliary line.
#' @param endX,endY End coordinates for auxiliary line.
#' @param color Color for auxiliary line
#' @param width Width for auxiliary line
#' @param type Type for auxiliary line, could be straight/curve/broken
#' @param lineType Line type, could be solid/dot
#' @param controlPointX,controlPointY The middle point coordinates for curve and broken
#' @param dashArray The dash gap width
#' @param marker Whether display a marker on the end of line
#' @param markerType Type of marker, could be circle/square/arrow/stub
#' @param markerColor,markerHeight,markerWidth Color, Height and Width for marker
#' @param markerPosition 1 means start, 2 means end, 3 means both
#' @param animationDisplay whether display animation
#' @param animationTime,animationDelay,animationType The time, delay and display type for animation
#'
#' @param ... Ignored
#'
#' @examples
#' NGCircos(NGCircosAuxLineTrack('AuxLineTrack01'))
#'
#' @export
NGCircosAuxLineTrack <- function(trackname, startX = 20, startY = 20, endX = 120, endY = 120, color = "red", width = 0.5,
                                type = "straight", controlPointX = 0, controlPointY = 0, lineType = "solid",
                                dashArray = 3, marker = TRUE, markerType = "circle", markerColor = "blue",
                                markerHeight = 5, markerWidth = 5, markerPosition = 2, animationDisplay = FALSE,
                                animationTime = 50, animationDelay = 1000, animationType = "linear", ...){
  track1 = paste("AUXILIARYLINE", trackname, sep="_")
  track2 = list(startX = startX, startY = startY, endX = endX, endY = endY, AUXILIARYLINEColor = color,
                AUXILIARYLINEWidth = width, AUXILIARYLINEType = type, AUXILIARYLINEControlPointX = controlPointX,
                AUXILIARYLINEControlPointY = controlPointY, AUXILIARYLINELineType = lineType,
                AUXILIARYLINEDashArray = dashArray, AUXILIARYLINEMarker = marker, AUXILIARYLINEMarkerType = markerType,
                AUXILIARYLINEMarkerColor = markerColor, AUXILIARYLINEMarkerHeight = markerHeight,
                AUXILIARYLINEMarkerWidth = markerWidth, AUXILIARYLINEMarkerPosition = markerPosition,
                AUXILIARYLINEanimationDisplay = animationDisplay, AUXILIARYLINEAnimationTime = animationTime,
                AUXILIARYLINEAnimationDelay = animationDelay, AUXILIARYLINEAnimationType = animationType)
  track = NGCircosTracklist() + list(list(track1, track2))
  return(track)
}

#' @title Create a CNV module to a NGCircos tracklist
#'
#' @description A copy number variance track displayed in the visualization
#'
#' @param trackname The name of the new track.
#'
#' @param compareGroup The group number of thic track in compare module
#' @param maxRadius,minRadius Where the track should begin and end.
#' @param width Width for CNV track
#' @param color Color for CNV track
#' @param ValueAxisManualScale Whether manually control the scale of value
#' @param ValueAxisMaxScale,ValueAxisMinScale The max and min scale value for manually control
#' @param strokeColor,strokeWidth The color and width for stroke
#' @param opacity The opacity for track
#' @param animationDisplay Whether display animation
#' @param animationTime,animationDelay,animationType The time, delay and display type for animation
#' @param data A list of CNV with details including start, end, value, link, color and html.
#'             Details can be found on NGCircos document.
#'
#' @param ... Ignored
#'
#' @examples
#' cnvData<-cnvExample
#' NGCircos(NGCircosCnvTrack('CnvTrack01',maxRadius =175, minRadius =116, data =cnvData,width=2,color = "#4876FF")+
#' NGCircosBackgroundTrack("bgTrack01",minRadius = 116,maxRadius = 175,fillColors = "#F2F2F2",axisShow = TRUE),CNVMouseOverDisplay = TRUE)
#'
#' @export
NGCircosCnvTrack <- function(trackname, compareGroup = 1, maxRadius = 200, minRadius = 190, width = 10, color = "#CAE1FF",
                            ValueAxisManualScale = FALSE, ValueAxisMaxScale = 10, ValueAxisMinScale = 0,
                            strokeColor = "black", strokeWidth = 1, opacity = 1, animationDisplay = FALSE,
                            animationTime = 2000, animationDelay = 50, animationType = "bounce", data, ...){
  track1 = paste("CNV", trackname, sep="_")
  track2 = list(compareGroup = compareGroup, maxRadius = maxRadius, minRadius = minRadius, CNVwidth = width,
                CNVColor = color, ValueAxisManualScale = ValueAxisManualScale, ValueAxisMaxScale = ValueAxisMaxScale,
                ValueAxisMinScale = ValueAxisMinScale, strokeColor = strokeColor, strokeWidth = strokeWidth,
                opacity = opacity, CNVAnimationDisplay = animationDisplay, CNVAnimationTime = animationTime,
                CNVAnimationDelay = animationDelay, CNVAnimationType = animationType)
  track3 = unname(alply(data, 1, as.list))
  track = NGCircosTracklist() + list(list(track1, track2, track3))
  return(track)
}

#' @title Cnv module example data
#' @description The data is in matrix with row names
#'
#' @format A data frame with 7 columns:
#' \describe{
#'   \item{chr}{chromosome}
#'   \item{start}{start position}
#'   \item{end}{end position}
#'   \item{value}{value}
#'   \item{link}{hyperlink for cnv}
#'   \item{color}{color}
#'   \item{html}{The external html language}
#' }
"cnvExample"

#' @title Create a HEATMAP module to a NGCircos tracklist
#'
#' @description A heatmap plot displayed in the visualization
#'
#' @param trackname The name of the new track.
#'
#' @param compareGroup The group number of thic track in compare module
#' @param maxRadius,minRadius Where the track should begin and end.
#' @param minColor The color for heatmap with min value
#' @param maxColor The color for heatmap with max value
#' @param ValueAxisManualScale Whether manually control the scale of value
#' @param ValueAxisMaxScale,ValueAxisMinScale The max and min scale value for manually control
#' @param totalLayer The color and width for stroke
#' @param animationDisplay Whether display animation
#' @param animationDirection The direction for animation. O2I: from outside to inside, I2O: from inside to outside
#' @param animationColorDirection The color changing in animation. L2C: lowest to customized, H2C: highest to customized, the customized color should be defined in data
#' @param animationTime,animationDelay,animationType The time, delay and display type for animation
#' @param data A list of value in heatmap plot with details including chr, start, end, value, name, layer and html.
#'             Details can be found on NGCircos document.
#'
#' @param ... Ignored
#'
#' @examples
#' heatmapData<-heatmapExample
#' NGCircos(NGCircosHeatmapTrack('HeatmapTrack01', maxRadius= 180, minRadius = 100, data=heatmapData,totalLayer = 3),
#' genome = list("2L"=23011544,"2R"=21146708,"3L"=24543557,"3R"=27905053,"4"=1351857,"X"=22422827),
#' HEATMAPMouseEvent = TRUE,HEATMAPMouseOverDisplay = TRUE)
#'
#' @export
NGCircosHeatmapTrack <- function(trackname, compareGroup = 1, maxRadius = 180, minRadius = 100, minColor = "red",
                                maxColor = "green", ValueAxisManualScale = FALSE, ValueAxisMaxScale = 10,
                                ValueAxisMinScale = 0, totalLayer = 1, animationDisplay = FALSE,
                                animationDirection = "O2I", animationColorDirection = "L2C",
                                animationTime = 2000, animationDelay = 20, animationType = "bounce", data, ...){
  track1 = paste("HEATMAP", trackname, sep="_")
  track2 = list(compareGroup = compareGroup, outerRadius = maxRadius, innerRadius = minRadius, maxColor = maxColor,
                minColor = minColor, ValueAxisManualScale = ValueAxisManualScale, ValueAxisMaxScale = ValueAxisMaxScale,
                ValueAxisMinScale = ValueAxisMinScale, totalLayer = totalLayer, HEATMAPAnimationDisplay = animationDisplay,
                HEATMAPAnimationDirection = animationDirection, HEATMAPAnimationColorDirection = animationColorDirection,
                HEATMAPAnimationTime = animationTime, HEATMAPAnimationDelay = animationDelay,
                HEATMAPAnimationType = animationType)
  track3 = unname(alply(data, 1, as.list))
  track = NGCircosTracklist() + list(list(track1, track2, track3))
  return(track)
}

#' @title Heatmap plot example data
#' @description The data is in matrix with row names
#'
#' @format A data frame with 7 columns:
#' \describe{
#'   \item{chr}{chromosome}
#'   \item{start}{start position}
#'   \item{end}{end position}
#'   \item{name}{name for description}
#'   \item{value}{value}
#'   \item{layer}{layer number}
#'   \item{html}{The external html language}
#' }
"heatmapExample"

#' @title Create a BUBBLE module to a NGCircos tracklist
#'
#' @description A bubble plot displayed in the visualization
#'
#' @param trackname The name of the new track.
#'
#' @param compareGroup The group number of thic track in compare module
#' @param maxRadius,minRadius Where the track should begin and end.
#' @param blockStroke Whether display the stroke between each bubble block
#' @param blockStrokeColor Stroke color for block
#' @param blockStrokeWidth Stroke width for block
#' @param blockFill Whether fill a block or not
#' @param blockFillColor The color for filling the block
#' @param bubbleMaxSize The max size for bubble
#' @param bubbleMinSize The min size for bubble
#' @param maxColor The color the bubble with max value
#' @param minColor The color the bubble with min value
#' @param ValueAxisManualScale Whether manually control the scale of value
#' @param ValueAxisMaxScale,ValueAxisMinScale The max and min scale value for manually control
#' @param totalLayer The color and width for stroke
#' @param animationDisplay Whether display animation
#' @param animationTime,animationDelay,animationType The time, delay and display type for animation
#' @param data A list of value in bubble plot with details including chr, start, end, value, name, layer, color and html.
#'             Details can be found on NGCircos document.
#'
#' @param ... Ignored
#'
#' @examples
#' bubbleData<-bubbleExample
#' NGCircos(NGCircosBubbleTrack('BubbleTrack01', maxRadius = 230, minRadius = 170, data=bubbleData, blockStroke = TRUE,
#' bubbleMaxSize =10, bubbleMinSize = 2, maxColor = "red", minColor = "yellow", totalLayer =3, animationDisplay = TRUE,
#' animationType="linear"),genome = list("2L"=23011544,"2R"=21146708,"3L"=24543557,"3R"= 27905053,"X"=22422827,"4"=1351857),
#' BUBBLEMouseOverDisplay =TRUE,innerRadius = 236)
#'
#' @export
NGCircosBubbleTrack <- function(trackname, compareGroup = 1, maxRadius = 200, minRadius = 50, blockStroke = TRUE,
                                blockStrokeColor = "black", blockStrokeWidth = 1, blockFill = FALSE,
                                blockFillColor = "white", bubbleMaxSize = 5, bubbleMinSize = 2, minColor = "red",
                                maxColor = "green", ValueAxisManualScale = FALSE, ValueAxisMaxScale = 10,
                                ValueAxisMinScale = 0, totalLayer = 1, animationDisplay = FALSE,
                                animationTime = 2000, animationDelay = 20, animationType = "bounce", data, ...){
  track1 = paste("BUBBLE", trackname, sep="_")
  track2 = list(compareGroup = compareGroup, maxRadius = maxRadius, minRadius = minRadius, blockStroke = blockStroke,
                blockStrokeColor = blockStrokeColor, blockStrokeWidth = blockStrokeWidth, blockFill = blockFill,
                blockFillColor = blockFillColor, bubbleMaxSize = bubbleMaxSize, bubbleMinSize = bubbleMinSize,
                maxColor = maxColor, minColor = minColor, ValueAxisManualScale = ValueAxisManualScale,
                ValueAxisMaxScale = ValueAxisMaxScale, ValueAxisMinScale = ValueAxisMinScale, totalLayer = totalLayer,
                BUBBLEAnimationDisplay = animationDisplay, BUBBLEAnimationTime = animationTime,
                BUBBLEAnimationDelay = animationDelay, BUBBLEAnimationType = animationType)
  track3 = unname(alply(data, 1, as.list))
  track = NGCircosTracklist() + list(list(track1, track2, track3))
  return(track)
}

#' @title Bubble plot example data
#' @description The data is in matrix with row names
#'
#' @format A data frame with 8 columns:
#' \describe{
#'   \item{chr}{chromosome}
#'   \item{start}{start position}
#'   \item{end}{end position}
#'   \item{name}{name for description}
#'   \item{value}{value}
#'   \item{color}{specified color for bubble}
#'   \item{layer}{layer number}
#'   \item{html}{The external html language}
#' }
"bubbleExample"


#' @title Create a GENE module to a NGCircos tracklist
#'
#' @description A number of genes with different functional region displayed in the visualization
#'
#' @param trackname The name of the new track.
#'
#' @param compareGroup The group number of thic track in compare module
#' @param outerRadius,innerRadius Where the track should begin and end.
#' @param pathColor The color for path between gene elements
#' @param pathWidth The width for path between gene elements
#' @param arrow Whether display arrows on path
#' @param arrowGap,arrowColor,arrowSize The gap, color and size for arrow
#' @param cdsColor,cdsStrokeColor,cdsStrokeWidth The color, stroke color and stroke width for coding
#' @param utrWidth,utrColor,utrStrokeColor,utrStrokeWidth The max size for bubble
#' @param animationDisplay Whether display animation
#' @param animationTime,animationDelay,animationType The time, delay and display type for animation
#' @param data A list of gene with details including chr, strand, start, end, type, name, link and html.
#'             Details can be found on NGCircos document.
#'
#' @param ... Ignored
#'
#' @examples
#' geneData<-geneExample
#' NGCircos(NGCircosGeneTrack('GeneTrack01', outerRadius = 195, innerRadius = 180, data=geneData,arrowGap = 10,
#'  arrowColor = "black",arrowSize = "12px",cdsColor = "#1e77b3",cdsStrokeColor = "#1e77b3",cdsStrokeWidth= 5,
#'  utrWidth= -2,utrColor= "#fe7f0e",utrStrokeColor= "#fe7f0e",animationDisplay = TRUE),genome =list("EGFR"=1000),
#'  outerRadius = 220)
#'
#' @export
NGCircosGeneTrack <- function(trackname, compareGroup = 1, outerRadius = 180, innerRadius = 150, pathColor = "black",
                              pathWidth = 1, arrow = TRUE, arrowGap = 2, arrowColor = "blue",
                              arrowSize = 5, cdsColor = "#1e77b3", cdsStrokeColor = "black",cdsStrokeWidth = 1,
                              utrWidth = -5, utrColor = "blue", utrStrokeColor = "blue", utrStrokeWidth = 1,
                              animationDisplay = FALSE,animationTime = 2000, animationDelay = 20,
                              animationType = "bounce", data, ...){
  track1 = paste("GENE", trackname, sep="_")
  track2 = list(compareGroup = compareGroup, outerRadius = outerRadius, innerRadius = innerRadius, pathColor = pathColor,
                pathWidth = pathWidth, arrow = arrow, arrowGap = arrowGap, arrowColor = arrowColor, arrowSize = arrowSize,
                cdsColor = cdsColor, cdsStrokeColor = cdsStrokeColor, cdsStrokeWidth = cdsStrokeWidth,
                utrWidth = utrWidth, utrColor = utrColor, utrStrokeColor = utrStrokeColor,utrStrokeWidth = utrStrokeWidth,
                GENEAnimationDisplay = animationDisplay, GENEAnimationTime = animationTime,
                GENEAnimationDelay = animationDelay, GENEAnimationType = animationType)
  track3 = unname(alply(data, 1, as.list))
  track = NGCircosTracklist() + list(list(track1, track2, track3))
  return(track)
}

#' @title Gene plot example data
#' @description The data is in matrix with row names
#'
#' @format A data frame with 8 columns:
#' \describe{
#'   \item{chr}{chromosome}
#'   \item{strand}{strand, - or +}
#'   \item{start}{start position}
#'   \item{end}{end position}
#'   \item{type}{region type, gene or utr or cds}
#'   \item{name}{name for description}
#'   \item{link}{hyperlink for this region}
#'   \item{html}{The external html language}
#' }
"geneExample"


#' @title Create a track with SNPs to be added to a NGCircos tracklist
#'
#' @description SNPs are defined by genomic coordinates and associated with a numerical value
#'
#' @param trackname The name of the new track.
#'
#' @param compareGroup The group number of thic track in compare module
#' @param maxRadius,minRadius Where the track should begin and end
#' @param fillColorType The type of filling color, could be either specific or r2(means based on r2)
#' @param fillColor If specific, the color for SNP filling
#' @param fillr2Color If r2, the color for SNP filling
#' @param ValueAxisManualScale Whether manually control the scale of value
#' @param ValueAxisMaxScale,ValueAxisMinScale The max and min scale value for manually control
#' @param pointType The type of SNP point, could be circle or rect
#' @param circleSize If circle, the size for SNP circle
#' @param rectWidth If rect, the width for SNP rect
#' @param rectHeight If rect, the height for SNP rect
#' @param animationDisplay Whether display animation
#' @param animationInitialPositionX,animationInitialPositionY The initial position coordinates for animation
#' @param animationTime,animationDelay,animationType The time, delay and display type for animation
#' @param data A list of SNP value with details including chr, pos, value, des, color, r2value, link, index, image and html.
#'             Details can be found on NGCircos document.
#'
#' @param ... Ignored
#'
#' @examples
#' snpData<-snpExample
#' NGCircos(NGCircosSnpTrack('SNPTrack', minRadius =150, maxRadius = 190, data = snpExample,fillColor= "#9ACD32",
#'    circleSize= 2, SNPAnimationDisplay=TRUE,SNPAnimationTime= 2000,SNPAnimationDelay= 0, SNPAnimationType= "linear") +
#'     NGCircosBackgroundTrack('BGTrack',minRadius = 145, maxRadius = 200))
#'
#' @export
NGCircosSnpTrack <- function(trackname, compareGroup = 1, minRadius = 153, maxRadius = 205, fillColorType = "specific",
                             fillColor = "#9400D3", fillr2Color = c("13#ff0031","#ff0031","#ff0031","#ff0031","#ff0031"),
                             ValueAxisManualScale= FALSE, ValueAxisMaxScale = 10, ValueAxisMinScale = 0,
                             pointType = "circle", circleSize = 2, rectWidth = 2, rectHeight = 2,
                             animationDisplay = FALSE, animationInitialPositionX = 0, animationInitialPositionY = 0,
                             animationTime = 2000, animationDelay = 20, animationType = "bounce", data, ...){

  track1 = paste("SNP", trackname, sep="_")
  track2 = list(compareGroup = compareGroup, minRadius = minRadius, maxRadius = maxRadius,
                SNPFillColorType = fillColorType, SNPFillColor = fillColor, SNPFillr2Color = fillr2Color,
                ValueAxisManualScale = ValueAxisManualScale, ValueAxisMaxScale = ValueAxisMaxScale,
                ValueAxisMinScale = ValueAxisMinScale, PointType = pointType, circleSize = circleSize,
                rectWidth = rectWidth, rectHeight = rectHeight, SNPAnimationDisplay = animationDisplay,
                SNPAnimationInitialPositionX = animationInitialPositionX, SNPAnimationInitialPositionY = animationInitialPositionY,
                SNPAnimationTime = animationTime, SNPAnimationDelay = animationDelay,
                SNPAnimationType = animationType)

  track3 = unname(alply(data, 1, as.list))

  track = NGCircosTracklist() + list(list(track1, track2, track3))
  return(track)
}

#' @title Snp plot example data
#' @description The data is in matrix with row names
#'
#' @format A data frame with 10 columns:
#' \describe{
#'   \item{chr}{chromosome}
#'   \item{pos}{position}
#'   \item{value}{value,such as p-value}
#'   \item{des}{description}
#'   \item{color}{color}
#'   \item{r2value}{r2 value}
#'   \item{link}{hyperlink for snp}
#'   \item{index}{index for combination}
#'   \item{image}{image for combination}
#'   \item{html}{The external html language}
#' }
"snpExample"



#' @title Create a LINK module to a NGCircos tracklist
#'
#' @description Link two specific region in genome.
#'
#' @param trackname The name of the new track.
#'
#' @param compareGroup The group number of thic track in compare module
#' @param radius Radius of link circle.
#' @param fillColor Color for link.
#' @param width Width for link.
#' @param type Type of link, could be Q/S/T
#' @param displayLinkAxis Whether display axis for link or not
#' @param axisColor The color for axis
#' @param axisWidth The width for axis
#' @param axisPad The pad for axis
#' @param displayLinkLabel Whether display label for link or not
#' @param labelColor The color for label
#' @param labelSize The size for label
#' @param labelPad The pad for label
#' @param animationDisplay Whether display animation
#' @param animationDirection The direction of link animation, could be 1to2 or 2to1
#' @param animationTime,animationDelay,animationType The time, delay and display type for animation
#' @param data A list of link with details including g1chr, g1start, g1end, g2chr, g2start, g2end, g1name, g2name,
#'             fusion, link and html. Details can be found on NGCircos document.
#'
#' @param ... Ignored
#'
#' @examples
#' linkData<-linkExample
#' NGCircos(NGCircosLinkTrack('LINKTrack', data = linkData,LinkRadius= 140,fillColor= "#9e9ac6",width= 2,
#' axisPad= 3,labelPad=8,animationDisplay=TRUE,animationDirection="1to2", animationType= "linear" ))
#'
#' @export
NGCircosLinkTrack <- function(trackname, compareGroup = 1, radius = 108, fillColor = "red", width = 3,
                              type = "Q", displayLinkAxis = TRUE, axisColor = "#B8B8B8", axisWidth = 0.5,
                              axisPad = 3, displayLinkLabel = TRUE, labelColor = "red", labelSize = 13,
                              labelPad = 8, animationDisplay = FALSE, animationDirection = "1to2",
                              animationTime = 2000, animationDelay = 20, animationType = "bounce", data, ...){

  track1 = paste("LINK", trackname, sep="_")
  track2 = list(compareGroup = compareGroup, LinkRadius = radius, LinkFillColor = fillColor,
                LinkWidth = width, LinkType = type, displayLinkAxis = displayLinkAxis,
                LinkAxisColor = axisColor, LinkAxisWidth = axisWidth, LinkAxisPad = axisPad,
                displayLinkLabel = displayLinkLabel, LinkLabelColor = labelColor, LinkLabelSize = labelSize,
                LinkLabelPad = labelPad, LINKAnimationDisplay = animationDisplay,
                LINKAnimationDirection = animationDirection,LINKAnimationTime = animationTime,
                LINKAnimationDelay = animationDelay,LINKAnimationType = animationType)

  track3 = unname(alply(data, 1, as.list))

  track = NGCircosTracklist() + list(list(track1, track2, track3))
  return(track)
}

#' @title Link plot example data
#' @description The data is in matrix with row names
#'
#' @format A data frame with 11 columns:
#' \describe{
#'   \item{g1chr}{first chromosome}
#'   \item{g1start}{first start position}
#'   \item{g1end}{first end position}
#'   \item{g2chr}{second chromosome}
#'   \item{g2start}{second start position}
#'   \item{g2end}{second end position}
#'   \item{g1name}{first name}
#'   \item{g2name}{second name}
#'   \item{fusion}{fusion name}
#'   \item{link}{hyperlink for link line}
#'   \item{html}{The external html language}
#' }
"linkExample"


#' @title Create a CHORD module to a NGCircos tracklist
#'
#' @description Display a chord module using a data matrix.
#'
#' @param trackname The name of the new track.
#'
#' @param innerRadius The inner radius for chord circle
#' @param outerRadius The outer radius for chord circle
#' @param fillOpacity The opacity for filling color.
#' @param fillStrokeWidth The stroke width for chord.
#' @param padding The pad of chord
#' @param autoFillColor Whether auto assign color for chord
#' @param fillColor If not, manually assign color for chord
#' @param fillStrokeColor The color for stroke
#' @param outerARC Whether display outer arc
#' @param outerARCAutoColor If true, whether auto assign color for arc
#' @param outerARCColor The manullay assigned color for arc
#' @param outerARCStrokeColor The stroke color for arc
#' @param outerARCText Whether display text for arc or not
#' @param data A matrix-list of chord value with relationship details.
#'
#' @param ... Ignored
#'
#' @examples
#'
#' chordData<-chordExample
#' NGCircos(NGCircosChordTrack('CHORDTrack', data = chordData,innerRadius= 210,outerRadius= 211,fillOpacity=0.67,
#' strokeColor="black",strokeWidth= "1px",outerARCText=FALSE),genome=list("C.CK" = 189.51,"C.NPK"=188,"GC.CK"=186.11,
#' "GC.NPK"=191.51,"Alphaproteobacteria"=70.16,"Betaproteobacteria"=23.51,"Gammaproteobacteria"=25.51,
#' "Deltaproteobacteria"=23.28,"Acidobacteria"=53.62,"Actinobacteria"=72.33,"Bacteroidetes"=22.41,
#' "Chloroflexi"=15.08,"Firmicutes"=10.72,"Gemmatimonadetes"=26.37,"Planctomycetes"=19.26,"Thaumarchaeota"=6.15,
#' "Verrucomicrobia"=8.3,"Ascomycota"=159.41,"Basidiomycota"=79.73,"Zygomycota"=139.29 ),outerRadius = 217,
#' genomeLabelDisplay = FALSE)
#'
#' @export
NGCircosChordTrack <- function(trackname, innerRadius = 237, outerRadius = 238, fillOpacity = 0.67, fillStrokeWidth = 1,
                               padding = 0.06, autoFillColor = TRUE, fillColor = c("#B8B8B8"), fillStrokeColor = c("black"),
                               outerARC = TRUE, outerARCAutoColor = TRUE, outerARCColor = c("red"),
                               outerARCStrokeColor = c("black"), outerARCText = TRUE, data, ...){

  track1 = paste("CHORD", trackname, sep="_")
  track2 = list(CHORDinnerRadius = innerRadius, CHORDouterRadius = outerRadius, CHORDFillOpacity = fillOpacity,
                CHORDFillStrokeWidth = fillStrokeWidth, CHORDPadding = padding, CHORDAutoFillColor = autoFillColor,
                CHORDFillColor = fillColor, CHORDFillStrokeColor = fillStrokeColor, CHORDouterARC = outerARC,
                CHORDouterARCAutoColor = outerARCAutoColor, CHORDouterARCColor = outerARCColor,
                CHORDouterARCStrokeColor = outerARCStrokeColor, CHORDouterARCText = outerARCText)


  track3 = list(colnames(data))
  tmp<-list(as.numeric(unname(data[1,])))
  for(i in 2:nrow(data)){
    tmp<-c(tmp,list(as.numeric(unname(data[i,]))))
  }
  track3 = c(track3,list(tmp))

  track = NGCircosTracklist() + list(list(track1, track2, track3))
  return(track)
}

#' @title Chord plot example data
#' @description The data is in matrix with row names
#'
#' @format A data frame in which each value represents the relationship from a column to a row:
#' \describe{
#'   \item{column name}{the name for each arc}
#'   \item{row}{the order and number is same as column, representing the same items}
#' }
"chordExample"

#' @title Create a HISTOGRAM module to a NGCircos tracklist
#'
#' @description Display a multi-layer histogram in circos
#'
#' @param trackname The name of the new track.
#'
#' @param compareGroup The group number of thic track in compare module
#' @param maxRadius,minRadius Where the track should begin and end
#' @param ValueAxisManualScale Whether manually control the scale of value
#' @param ValueAxisMaxScale,ValueAxisMinScale The max and min scale value for manually control
#' @param fillColor The color for histgram.
#' @param animationDisplay Whether display animation
#' @param animationTime,animationDelay The time and delay for animation
#' @param data A list of value with details including chr, start, end, name, link, value and html. Details can be found on NGCircos document.
#'
#' @param ... Ignored
#'
#' @examples
#'
#' histogramData<-histogramExample
#' NGCircos(NGCircosHistogramTrack('HISTOGRAMTrack', data = histogramData,fillColor= "#ff7f0e",maxRadius = 210,
#' minRadius = 175),genome=list("2L"=23011544,"2R"=21146708,"3L"=24543557,"3R"= 27905053,"X"=22422827,"4"=1351857),
#' outerRadius = 220)
#'
#' @export
NGCircosHistogramTrack <- function(trackname, compareGroup = 1, maxRadius = 108, minRadius = 95,
                                   ValueAxisManualScale = FALSE, ValueAxisMaxScale = 10, ValueAxisMinScale = 0,
                                   fillColor = "red", animationDisplay = FALSE, animationTime = 2000,
                                   animationDelay = 20, data, ...){

  track1 = paste("HISTOGRAM", trackname, sep="_")
  track2 = list(compareGroup = compareGroup, maxRadius = maxRadius, minRadius = minRadius,
                ValueAxisManualScale = ValueAxisManualScale, ValueAxisMaxScale = ValueAxisMaxScale,
                ValueAxisMinScale = ValueAxisMinScale, histogramFillColor = fillColor,
                HISTOGRAMAnimationDisplay = animationDisplay, HISTOGRAMAnimationTime = animationTime,
                HISTOGRAMAnimationDelay = animationDelay)

  track3 = unname(alply(data, 1, as.list))

  track = NGCircosTracklist() + list(list(track1, track2, track3))
  return(track)
}

#' @title Histogram plot example data
#' @description The data is in matrix with row names
#'
#' @format A data frame with 7 columns:
#' \describe{
#'   \item{chr}{chromosome}
#'   \item{start}{start position}
#'   \item{end}{end position}
#'   \item{name}{name for description}
#'   \item{link}{hyperlink}
#'   \item{value}{value}
#'   \item{html}{The external html language}
#' }
"histogramExample"

#' @title Create a LINE module to a NGCircos tracklist
#'
#' @description Display a multi-layer line plot in circos
#'
#' @param trackname The name of the new track.
#'
#' @param compareGroup The group number of thic track in compare module
#' @param maxRadius,minRadius Where the track should begin and end
#' @param ValueAxisManualScale Whether manually control the scale of value
#' @param ValueAxisMaxScale,ValueAxisMinScale The max and min scale value for manually control
#' @param color Color for line
#' @param width Width for line
#' @param type Type for line, could be linear, cardinal, basis and monotone
#' @param animationDisplay Whether display animation
#' @param animationDirection The direction of animation, could be S2E(start to end) or E2S(end to start)
#' @param animationTime,animationDelay,animationType The time, delay and display type for animation
#' @param data A list of value with details including chr, pos, des, value and html. Details can be found on NGCircos document.
#'
#' @param ... Ignored
#'
#' @examples
#'
#' lineData<-lineExample
#' NGCircos(NGCircosLineTrack('LINETrack', data = lineData,maxRadius=200,minRadius=150,color= "#ff0031")+
#' NGCircosBackgroundTrack('BGTrack',minRadius = 205,maxRadius = 150))
#'
#' @export
NGCircosLineTrack <- function(trackname, compareGroup = 1, maxRadius = 108, minRadius = 95,
                                   ValueAxisManualScale = FALSE, ValueAxisMaxScale = 10, ValueAxisMinScale = 0,
                                   color = "red", width = 2, type = "cardinal",animationDisplay = FALSE,
                                   animationDirection = "S2E", animationTime = 2000,
                                   animationDelay = 20, animationType = "bounce", data, ...){

  track1 = paste("LINE", trackname, sep="_")
  track2 = list(compareGroup = compareGroup, maxRadius = maxRadius, minRadius = minRadius,
                ValueAxisManualScale = ValueAxisManualScale, ValueAxisMaxScale = ValueAxisMaxScale,
                ValueAxisMinScale = ValueAxisMinScale, LineColor = color, LineWidth = width, LineType = type,
                LINEAnimationDisplay = animationDisplay, LINEAnimationDirection = animationDirection,
                LINEAnimationTime = animationTime, LINEAnimationDelay = animationDelay,
                LINEAnimationType = animationType)

  track3 = unname(alply(data, 1, as.list))

  track = NGCircosTracklist() + list(list(track1, track2, track3))
  return(track)
}

#' @title Line plot example data
#' @description The data is in matrix with row names
#'
#' @format A data frame with 5 columns:
#' \describe{
#'   \item{chr}{chromosome}
#'   \item{pos}{position}
#'   \item{des}{description}
#'   \item{value}{value}
#'   \item{html}{The external html language}
#' }
"lineExample"

#' @title Create a WIG module to a NGCircos tracklist
#'
#' @description Display a multi-layer line plot in circos
#'
#' @param trackname The name of the new track.
#'
#' @param compareGroup The group number of thic track in compare module
#' @param maxRadius,minRadius Where the track should begin and end
#' @param direction The direction of plot, either inside or outside
#' @param ValueAxisManualScale Whether manually control the scale of value
#' @param ValueAxisMaxScale,ValueAxisMinScale The max and min scale value for manually control
#' @param color Color for plot
#' @param opacity Opacity for plot
#' @param strokeColor The color for stroke
#' @param strokeWidth The width for stroke
#' @param strokeType Line type for stroke, could be linear, cardinal, basis and monotone
#' @param animationDisplay Whether display animation
#' @param animationTime,animationDelay,animationType The time, delay and display type for animation
#' @param data A list of value with details including chr, pos, des, value and html. Details can be found on NGCircos document.
#'
#' @param ... Ignored
#'
#' @examples
#'
#' wigData<-wigExample
#' NGCircos(NGCircosWigTrack('WIGTrack', data = wigData, maxRadius= 200,minRadius= 150,strokeColor= "darkblue",
#' color= "lightblue",strokeType= "cardinal")+NGCircosBackgroundTrack('BGTrack',minRadius = 205,maxRadius = 150)
#' ,genome=list("chr8"=1000),outerRadius = 220)
#'
#' @export
NGCircosWigTrack <- function(trackname, compareGroup = 1, maxRadius = 108, minRadius = 95, direction = "out",
                             ValueAxisManualScale = FALSE, ValueAxisMaxScale = 10, ValueAxisMinScale = 0,
                             color = "red", opacity = 1, strokeColor = "black", strokeWidth = 1,
                             strokeType = "cardinal",animationDisplay = FALSE, animationTime = 2000,
                             animationDelay = 20, animationType = "bounce", data, ...){

  track1 = paste("WIG", trackname, sep="_")
  track2 = list(compareGroup = compareGroup, maxRadius = maxRadius, minRadius = minRadius, direction = direction,
                ValueAxisManualScale = ValueAxisManualScale, ValueAxisMaxScale = ValueAxisMaxScale,
                ValueAxisMinScale = ValueAxisMinScale, WIGColor = color, WIGOpacity = opacity,
                WIGStrokeColor = strokeColor, WIGStrokeWidth = strokeWidth, WIGStrokeType = strokeType,
                WIGAnimationDisplay = animationDisplay, WIGAnimationTime = animationTime,
                WIGAnimationDelay = animationDelay, WIGAnimationType = animationType)

  track3 = unname(alply(data, 1, as.list))

  track = NGCircosTracklist() + list(list(track1, track2, track3))
  return(track)
}

#' @title Wig plot example data
#' @description The data is in matrix with row names
#'
#' @format A data frame with 5 columns:
#' \describe{
#'   \item{chr}{chromosome}
#'   \item{pos}{position}
#'   \item{des}{description}
#'   \item{value}{value}
#'   \item{html}{The external html language}
#' }
"wigExample"

#' @title Create a SCATTER module to a NGCircos tracklist
#'
#' @description Display a point plot in circos
#'
#' @param trackname The name of the new track.
#'
#' @param compareGroup The group number of thic track in compare module
#' @param radius Radius of scatter circle
#' @param innerPointType,outerPointType The type for inner and outer point, could be circle or rect
#' @param innerCircleSize,outerCircleSize If circle, inner and outer circle size
#' @param innerCircleColor,outerCircleColor If circle, inner and outer circle color
#' @param outerCircleOpacity If circle, the opacity for outer circle
#' @param innerrectWidth,innerrectHeight If rect, inner width and height
#' @param outerrectWidth,outerrectHeight If rect, inner width and height
#' @param random_data Scatter position fluctuation
#' @param animationDisplay Whether display animation
#' @param animationInitialPositionX,animationInitialPositionY The initial coordinates for animation
#' @param animationTime,animationDelay,animationType The time, delay and display type for animation
#' @param data A list of value with details including chr, start, end, name, des, link and html.
#'             Details can be found on NGCircos document.
#'
#' @param ... Ignored
#'
#' @examples
#'
#' scatterData<-scatterExample
#' NGCircos(NGCircosScatterTrack('SCATTERTrack', data = scatterData,radius=180,innerCircleColor= "#3d6390",
#' outerCircleColor= "#99cafe",random_data= 40))
#'
#' @export
NGCircosScatterTrack <- function(trackname, compareGroup = 1, radius = 140, innerCircleSize = 1,
                                 outerCircleSize = 5, innerCircleColor = "#F26223", outerCircleColor = "#F26223",
                                 innerPointType = "circle", outerPointType = "circle", innerrectWidth = 2,
                                 innerrectHeight = 2, outerrectWidth = 2, outerrectHeight = 2,
                                 outerCircleOpacity = 1, random_data = 0, animationDisplay = FALSE,
                                 animationInitialPositionX = 0, animationInitialPositionY = 0, animationTime = 2000,
                                 animationDelay = 20, animationType = "bounce", data, ...){

  track1 = paste("SCATTER", trackname, sep="_")
  track2 = list(compareGroup = compareGroup, SCATTERRadius = radius, innerCircleSize = innerCircleSize,
                outerCircleSize = outerCircleSize, innerCircleColor = innerCircleColor,
                outerCircleColor = outerCircleColor, innerPointType = innerPointType,
                outerPointType = outerPointType, innerrectWidth = innerrectWidth,
                innerrectHeight = innerrectHeight, outerrectWidth = outerrectWidth, outerrectHeight = outerrectHeight,
                outerCircleOpacity = outerCircleOpacity, random_data = random_data,
                SCATTERAnimationDisplay = animationDisplay, SCATTERAnimationInitialPositionX = animationInitialPositionX,
                SCATTERAnimationInitialPositionY = animationInitialPositionY, SCATTERAnimationTime = animationTime,
                SCATTERAnimationDelay = animationDelay, SCATTERAnimationType = animationType)

  track3 = unname(alply(data, 1, as.list))

  track = NGCircosTracklist() + list(list(track1, track2, track3))
  return(track)
}

#' @title Scatter plot example data
#' @description The data is in matrix with row names
#'
#' @format A data frame with 7 columns:
#' \describe{
#'   \item{chr}{chromosome}
#'   \item{start}{start position}
#'   \item{end}{end position}
#'   \item{name}{name for scatter}
#'   \item{des}{description}
#'   \item{link}{hyperlink}
#'   \item{html}{The external html language}
#' }
"scatterExample"

#' @title Create a ARC module to a NGCircos tracklist
#'
#' @description Display the CNV without value, Gene domain, Chromosome band in the visualization
#'
#' @param trackname The name of the new track.
#'
#' @param compareGroup The group number of thic track in compare module
#' @param innerRadius,outerRadius Where the track should begin and end
#' @param opacity The opacity for arc
#' @param animationDisplay Whether display animation
#' @param animationTime,animationDelay,animationType The time, delay and display type for animation
#' @param data A list of arc with details including chr, start, end, color, des, link and html.
#'             Details can be found on NGCircos document.
#'
#' @param ... Ignored
#'
#' @examples
#' arcData<-arcExample
#' NGCircos(NGCircosArcTrack('ArcTrack01', outerRadius = 212, innerRadius = 224, data=arcData),
#'  genome=list("EGFR"=1211),outerRadius = 220,genomeFillColor = c("grey"))
#'
#' @export
NGCircosArcTrack <- function(trackname, compareGroup = 1, outerRadius = 150, innerRadius = 130, opacity = 1,
                              animationDisplay = FALSE,animationTime = 2000, animationDelay = 20,
                              animationType = "bounce", data, ...){
  track1 = paste("ARC", trackname, sep="_")
  track2 = list(compareGroup = compareGroup, outerRadius = outerRadius, innerRadius = innerRadius,
                ARCOpacity = opacity, ARCAnimationDisplay = animationDisplay, ARCAnimationTime = animationTime,
                ARCAnimationDelay = animationDelay, ARCAnimationType = animationType)
  track3 = unname(alply(data, 1, as.list))
  track = NGCircosTracklist() + list(list(track1, track2, track3))
  return(track)
}

#' @title Arc plot example data
#' @description The data is in matrix with row names
#'
#' @format A data frame with 7 columns:
#' \describe{
#'   \item{chr}{chromosome}
#'   \item{start}{start position}
#'   \item{end}{end position}
#'   \item{color}{color}
#'   \item{des}{description}
#'   \item{link}{hyperlink}
#'   \item{html}{The external html language}
#' }
"arcExample"


#' @title Create a LOLLIPOP module to a NGCircos tracklist
#'
#' @description Display a lollipop plot in the visualization
#'
#' @param trackname The name of the new track.
#'
#' @param compareGroup The group number of thic track in compare module
#' @param fillColor Filling color for lollipop
#' @param secondColor Second filling color for heterogeneous lollipop
#' @param pointType The type for lollipop, could be circle, rect and diamond
#' @param circleSize If circle, the size for lollipop
#' @param diamondWidth,diamondHeight If diamond, the width and height for lollipop
#' @param rectWidth,rectHeight If rect, the width and height for lollipop
#' @param stroke Whether display the stroke for lollipop
#' @param strokeColor,strokeWidth The color and width for stroke
#' @param lineAutoHeight Whether auto assign the height for each lollipop
#' @param lineAutoMaximumHeightZoomRate If auto assign, the zoom rate for each lollipop
#' @param lineHeightRate If manually assign, the rate of lollipop compared to real value
#' @param lineWidth,lineColor The width and color for the line of lollipop
#' @param realStart The real start position for data in genome.
#' @param ValueAxisManualScale Whether manually control the scale of value
#' @param ValueAxisMaxScale,ValueAxisMinScale The max and min scale value for manually control
#' @param animationDisplay Whether display animation
#' @param animationTime,animationDelay,animationType The time, delay and display type for animation
#' @param data A list of lollipop value with details including protein, chr, pos, strand, CancerTypeNumber, color, link,
#'             Consequence, AA_pos, AA_change, type, link and html.
#'             Details can be found on NGCircos document.
#'
#' @param ... Ignored
#'
#' @examples
#' lollipopData<-lollipopExample
#' arcData<-arcExample
#' NGCircos(NGCircosLollipopTrack('LollipopTrack01', data=lollipopData, fillColor="#9400D3",
#' circleSize= 6, strokeColor= "#999999", strokeWidth= "1px", animationDisplay=TRUE, lineWidth= 2,
#' realStart= 101219350)+NGCircosArcTrack('ArcTrack01', outerRadius = 212, innerRadius = 224, data=arcData),
#'  genome=list("EGFR"=1211),outerRadius = 220,genomeFillColor = c("grey"))
#'
#' @export
NGCircosLollipopTrack <- function(trackname, compareGroup = 1, fillColor = "#9400D3", secondColor = "#FFFFFF",
                                   pointType = "circle", circleSize = 2, diamondWidth = 10, diamondHeight = 5,
                                   rectWidth = 2, rectHeight = 2, stroke = TRUE, strokeColor = "#000000",
                                   strokeWidth = 0.5, lineAutoHeight = TRUE, lineAutoMaximumHeightZoomRate = 1,
                                   lineHeightRate = 0.75, lineWidth = 2, lineColor = "#000000", realStart = 0,
                                   ValueAxisManualScale = FALSE, ValueAxisMaxScale = 10,
                                   ValueAxisMinScale = 0, animationDisplay = FALSE, animationTime = 2000,
                                   animationDelay = 20, animationType = "bounce", data, ...){
  track1 = paste("LOLLIPOP", trackname, sep="_")
  track2 = list(compareGroup = compareGroup, LOLLIPOPFillColor = fillColor, LOLLIPOPSecondColor = secondColor,
                PointType = pointType, circleSize = circleSize, diamondWidth = diamondWidth,
                diamondHeight = diamondHeight, rectWidth = rectWidth, rectHeight = rectHeight,
                stroke = stroke, strokeColor = strokeColor, strokeWidth = strokeWidth, lineAutoHeight = lineAutoHeight,
                lineAutoMaximumHeightZoomRate = lineAutoMaximumHeightZoomRate, lineHeightRate = lineHeightRate,
                ValueAxisManualScale = ValueAxisManualScale, ValueAxisMaxScale = ValueAxisMaxScale,
                ValueAxisMinScale = ValueAxisMinScale, LOLLIPOPAnimationDisplay = animationDisplay,
                LOLLIPOPAnimationTime = animationTime, LOLLIPOPAnimationDelay = animationDelay,
                LOLLIPOPAnimationType = animationType, LOLLIPOPLineWidth = lineWidth,
                LOLLIPOPLineColor = lineColor, realStart = 0)
  track3 = unname(alply(data, 1, as.list))
  track = NGCircosTracklist() + list(list(track1, track2, track3))
  return(track)
}

#' @title Lollipop plot example data
#' @description The data is in matrix with row names
#'
#' @format A data frame with 12 columns:
#' \describe{
#'   \item{protein}{protein name}
#'   \item{chr}{chromosome}
#'   \item{pos}{position}
#'   \item{strand}{strand, - or +}
#'   \item{CancerTypeNumber}{Cancer type number}
#'   \item{color}{color}
#'   \item{link}{hyperlink}
#'   \item{Consequence}{consequence}
#'   \item{AA_pos}{AA_pos}
#'   \item{AA_change}{AA_change}
#'   \item{type}{type for mutation,Hetero or Homo}
#'   \item{html}{The external html language}
#' }
"lollipopExample"


#' @title Create a list of NGCircos tracks
#'
#' @description This allows the use of the '+' and '-' operator on these lists
#'
#' @name NGCircosTracklist
#'
#' @param x The tracklist on which other tracks should be added or removed.
#' @param ... The tracks to add (as tracklists) or to remove (as track names).
#'
#' @export
NGCircosTracklist <- function(){
  x = list()
  class(x) <- c("NGCircosTracklist")
  return(x)
}

#' @rdname NGCircosTracklist
#' @export
"+.NGCircosTracklist" <- function(x,...) {
  x <- append(x,...)
  if(class(x) != "NGCircosTracklist"){
    class(x) <- c("NGCircosTracklist")
  }
  return(x)
}

#' @rdname NGCircosTracklist
#' @export
"-.NGCircosTracklist" <- function(x,...) {
  indicesToDelete = list()
  for (i in 1:length(x)){
    if(paste(strsplit(x[[i]][[1]], '_')[[1]][-1], collapse = "_") %in% ...){
      indicesToDelete = append(indicesToDelete, i)
    }
  }
  y <- x
  y[unlist(indicesToDelete)] <- NULL
  return(y)
}

.NGCircosColorCheck <- function(colVar, colLength, varName = "Color") {
  # If genomeFillColor is a string, create corresponding palette
  colorError = paste0("\'", varName,
                      "\' parameter should be either a vector of chromosome colors or the name of a RColorBrewer brewer.")
  if(class(colVar) == "character"){
    if(all(colVar %in% rownames(RColorBrewer::brewer.pal.info))&(length(colVar) == 1)) { # RColorBrewer's brewer
      colVar = grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, colVar))(colLength)
      # colVar =list(colVar)
    }else if(length(colVar) == colLength){
      return(colVar)
    }else{
      print("Warning! The genomeFillColor should either has same length of genome or be a name of RColorBrewer brewer")
      colVar = grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Spectral"))(colLength)
    }
  }
  else{
    stop(colorError)
  }
  return(colVar)
}
