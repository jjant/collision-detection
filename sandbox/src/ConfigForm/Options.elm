module ConfigForm.Options exposing (ViewOptions, viewOptions, withFontSize, withInputSpacing, withInputWidth, withLabelHighlightBgColor, withRowSpacing, withSectionSpacing)

{-| Default options for viewing the config form.
-}

import Color exposing (Color)



-- VIEW OPTIONS


type alias ViewOptions =
    { fontSize : Int
    , rowSpacing : Int
    , inputWidth : Int
    , inputSpacing : Float
    , labelHighlightBgColor : Color
    , sectionSpacing : Int
    }


viewOptions : ViewOptions
viewOptions =
    { fontSize = 18
    , rowSpacing = 2
    , inputWidth = 80
    , inputSpacing = 1.4
    , labelHighlightBgColor = Color.rgb 0.8 0.8 1
    , sectionSpacing = 10
    }


{-| Update the font size in px. Default is 18.
-}
withFontSize : Int -> ViewOptions -> ViewOptions
withFontSize val options =
    { options | fontSize = val }


{-| Update the row spacing in px. Default is 2.
-}
withRowSpacing : Int -> ViewOptions -> ViewOptions
withRowSpacing val options =
    { options | rowSpacing = val }


{-| Update the width of inputs in px. Default is 80.
-}
withInputWidth : Int -> ViewOptions -> ViewOptions
withInputWidth val options =
    { options | inputWidth = val }


{-| Update the inner spacing of inputs by a ratio of its font size. Default is 1.40.
-}
withInputSpacing : Float -> ViewOptions -> ViewOptions
withInputSpacing val options =
    { options | inputSpacing = val }


{-| Update the row color when hovering field labels that are pointerlock-able. Default is yellow: (0.8, 0.8, 1).
-}
withLabelHighlightBgColor : Color -> ViewOptions -> ViewOptions
withLabelHighlightBgColor val options =
    { options | labelHighlightBgColor = val }


{-| Update the extra top spacing for sections in px. Default is 20.
-}
withSectionSpacing : Int -> ViewOptions -> ViewOptions
withSectionSpacing val options =
    { options | sectionSpacing = val }
