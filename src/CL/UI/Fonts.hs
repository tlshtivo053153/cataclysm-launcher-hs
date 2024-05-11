{-# LANGUAGE OverloadedStrings #-}
module CL.UI.Fonts
  ( tabFonts
  ) where

import CL.Define (AppModel(..), AppEvent(..))
import CL.Define.Fonts
import CL.MakeLenses

import Monomer

import qualified Data.Text as T

tabFonts :: WidgetNode AppModel AppEvent
tabFonts = vstack
  [ hstack
    [ vstack
      [ label "Available:"
      , selectList_ (fonts.available) ["font_A", "font_B"] label [onChange $ AppFonts . SelectAvailable]
      , button "Set for Game UI" $ AppFonts SetFontGame
      , button "Set for Map" $ AppFonts SetFontMap
      , button "Set for Overmap" $ AppFonts SetFontOvermap
      , button "Set for All" $ AppFonts SetFontAll
      , button "Reset All Fonts" $ AppFonts SetFontAll
      ]
    , separatorLine
      `styleBasic` [paddingL 10, paddingR 10]
    , vstack
      [ label "Preview:"
      , label_ "preview box" [ellipsis]
        `styleBasic`
          [ border 4 lightGray
          , radius 10
          , flexHeight 50
          ]

      , labeledCheckbox "Preview Cyrillic characters" (fonts.isPreviewCyrillic)
      , separatorLine
      , label "Other settings"
      , hstack
        [ label "UI font size"
        , numericField (fonts.fontSizeUI)
        ]
      , hstack
        [ label "Map font size"
        , numericField (fonts.fontSizeMap)
        ]
      , hstack
        [ label "Overmap font size"
        , numericField (fonts.fontSizeOvermap)
        ]
      , labeledCheckbox "font blending" (fonts.fontBlending)
      ]
    ]
  ]
