module Main where

import App
import Data.Bool
import JS
import Reflex.Dom

main :: IO ()
main = mainWidgetWithHead headWidget bodyWidget

headWidget :: MonadWidget t m => m ()
headWidget = do
  el "title" $ text "Cardano Mixer DApp"
  el "style" $ text
    ".wf-force-outline-none[tabindex=\"-1\"]:focus{outline:none;}"
  meta $ "charset" =: "utf-8"
  meta $ "content" =: "Cardano Mixer DApp" <> "property" =: "og:title"
  meta $ "content" =: "Cardano Mixer DApp" <> "property" =: "twitter:title"
  meta $ "content" =: "width=device-width, initial-scale=1" <>
    "name" =: "viewport"
  stylesheet "/css/normalize.css"
  stylesheet "/css/webflow.css"
  stylesheet "/css/cardanomixer-design-b1546-9b66de9866e02.webflow.css"
  eScriptLoaded <- domEvent Load . fst <$> elAttr' "script"
    ("src" =: "https://ajax.googleapis.com/ajax/libs/webfont/1.6.26/webfont.js"
      <> "type" =: "text/javascript") blank
  performEvent_ (runHeadScripts <$ eScriptLoaded)
  elAttr "link" ("rel" =: "stylesheet" <> "media" =: "all" <>
    "href" =: "http://fonts.googleapis.com/css?family=Droid+Serif:400,\
      \400italic,700,700italic%7CCorben:regular%7CFenix:regular") blank
  elAttr "link" ("href" =: "/images/favicon.png" <> "rel" =: "shortcut icon" <>
    "type" =: "image/x-icon") blank
  elAttr "link" ("href" =: "/images/webclip.jpg" <> "rel" =: "apple-touch-icon")
    blank
  elAttr "script" ("src" =: "/js/Nami.js" <> "type" =: "text/javascript") blank
  elAttr "script" ("src" =: "/js/static.js" <> "type" =: "text/javascript") blank
  where
    meta attr = elAttr "meta" attr blank
    stylesheet href = elAttr "link" ("href" =: href <> "rel" =: "stylesheet"
      <> "type" =: "text/css") blank

bodyWidget :: MonadWidget t m => m ()
bodyWidget = do
  navbarWidget
  logoWidget
  appWidget
  divClass "sectionlogo wf-section" .
    divClass "maincontainer w-container" . divClass "divblockcentered" $ do
    elAttr "p" ("class" =: "parwide" <> "align" =: "justify") .
        elAttr "strong" ("class" =: "bold-text-5") $ text "Notes:"
    elAttr "p" ("class" =: "parwide" <> "align" =: "justify") $
      text "1. The test will end in a few hours at 1 am UTC."
    elAttr "p" ("class" =: "parwide" <> "align" =: "justify") $
      text "2. We will publish a Medium blog on Monday discussing the test and the subsequent milestones."
    elAttr "p" ("class" =: "parwide" <> "align" =: "justify") $
      text "3. Depositing tMIX requires some additional tADA and tMIX."
    elAttr "p" ("class" =: "parwide" <> "align" =: "justify") $
      text "4. Withdrawal transactions take 3-5 minutes to be confirmed."
    elAttr "p" ("class" =: "parwide" <> "align" =: "justify") $
      text "5. Message \"Withdrawal data is not correct\" usually appears when you try to use the same key twice."
  footerWidget

navbarWidget :: MonadWidget t m => m ()
navbarWidget = elAttr "div" ("data-animation" =: "default" <>
  "data-collapse" =: "small" <> "data-duration" =: "400" <>
  "data-easing" =: "ease" <> "data-easing2" =: "ease" <>
  "role" =: "banner" <> "class" =: "navbar w-nav") $ do
    divClass "w-container" $ do
      elAttr "nav" ("role" =: "navigation" <> "class" =: "w-nav-menu") .
        mapM_ mkNavLink $
          [ (howWorksHref, "How does it work?", False)
          , (featuresHref, "Features", False)
          , (faqHref, "FAQ", False)
          , (paperHref, "White paper", True)
          , (homeHref, "Home", True) ]
      elAttr "img" ("src" =: "/images/Mainlogo.svg" <> "loading" =: "lazy" <>
        "width" =: "45" <> "alt" =: "" <> "class" =: "imagelightdark") blank
    elAttr "div" ("class" =: "w-nav-overlay" <> "data-wf-ignore" =: "" <>
      "id" =: "w-nav-overlay-0") blank
  where
    mkNavLink (href, txt, newTab) = elAttr "a" ("href" =: href <>
      "class" =: "nav-link w-nav-link" <> "style" =: "max-width: 940px;" <>
      bool mempty ("target" =: "_blank") newTab) $ text txt
    -- TODO: use real values
    howWorksHref = "#"
    featuresHref = "#"
    faqHref = "#"
    paperHref = "https://cardmix.io/whitepaper.pdf"
    homeHref = "#"

logoWidget :: MonadWidget t m => m ()
logoWidget = divClass "sectionlogo wf-section" .
  divClass "maincontainer w-container" . divClass "divblockcentered" $ do
    elAttr "img" ("src" =: "images/Mainlogo.png" <> "loading" =: "lazy" <>
      "width" =: "278" <> "sizes" =: "(max-width: 479px) 95vw, 278px" <>
      "srcset" =: "/images/Mainlogo-p-500.png 500w, images/Mainlogo-p-800.png\
        \ 800w, /images/Mainlogo.png 1112w" <> "alt" =: "" <>
      "class" =: "imagelogo") blank
    elClass "h2" "headinglogo" . text $ "protect your privacy in one click"

footerWidget :: MonadWidget t m => m ()
footerWidget = elAttr "div" ("id" =: "Footer" <>
  "class" =: "sectionfooter wf-section") .
    divClass "containerfooter w-container" $ do
      divClass "textjoinus" . text $ "Join  us!"
      mapM_ mkLink
        [ (tgLink, tgImg, True)
        , (discordLink, discordImg, True)
        , (twitterLink, twitterImg, True)
        , (ideascaleLink, ideascaleImg, True)
        , (mediumLink, mediumImg, True)
        , (githubLink, githubImg, False) ]
  where
    tgLink = "http://t.me/cardano_mixer"
    tgImg = "/images/telegram.svg"
    discordLink = "https://discord.gg/Q3gPP87Tcw"
    discordImg = "/images/discord.svg"
    twitterLink = "https://twitter.com/CardanoMixer"
    twitterImg = "/images/twitter.svg"
    ideascaleLink = "https://cardano.ideascale.com/a/dtd/369219-48088"
    ideascaleImg = "/images/IdeaScale.svg"
    mediumLink = "http://cardmix.medium.com/"
    mediumImg = "/images/Medium.svg"
    githubLink = "#"
    githubImg = "/images/GitInDev.svg"
    mkLink (href, img, isActive) = elAttr "a" ("href" =: href <>
      "target" =: "_blank" <>
      "class" =: bool "inactive-link-block w-inline-block" "w-inline-block"
        isActive) $ elAttr "img" ("src" =: img <> "loading" =: "lazy" <>
          "alt" =: "" <> "class" =: "pictogram") blank
