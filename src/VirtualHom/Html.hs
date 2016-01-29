{-# LANGUAGE OverloadedStrings #-}
{- Constructors for HTML elements -}
module VirtualHom.Html(
  -- * Content Sectioning
  address,
  article,
  footer,
  header,
  h1,
  h2,
  h3,
  h4,
  h5,
  h6,
  hgroup,
  nav,
  section,
  -- * Text content
  dd,
  div,
  dl,
  dt,
  figcaption,
  figure,
  hr,
  li,
  main,
  ol,
  p,
  pre,
  ul,
  -- * Inline text semantics
  abbr,
  b,
  bdi,
  bdo,
  br,
  cite,
  code,
  data_,
  dfn,
  em,
  i,
  kbd,
  mark,
  q,
  rp,
  rt,
  rtc,
  ruby,
  s,
  samp,
  small,
  span,
  strong,
  sub,
  sup,
  time,
  u ,
  var_,
  wbr,
  -- * Image and multimedia
  area,
  audio,
  map_,
  track,
  video,
  -- * Embedded content
  embed,
  object,
  param,
  source,
  -- * Scripting
  canvas,
  noscript,
  script,
  -- * Demarcating edits
  del,
  ins,
  -- * Table content
  caption,
  col,
  colgroup,
  table,
  tbody,
  td,
  tfoot,
  th,
  thead,
  tr,
  -- * Forms
  button,
  datalist,
  fieldset,
  form,
  input,
  keygen,
  label,
  legend,
  meter,
  optgroup,
  option,
  output,
  progress,
  select,
  -- * Interactive elements
  details,
  dialog,
  menu,
  menuitem,
  summary,
  -- * Web components
  content,
  element,
  shadow,
  template
  ) where

import VirtualHom.Internal.Element hiding (content, input, select)
import qualified VirtualHom.Internal.Element as E

import Control.Lens hiding (pre, element)
import Data.Text (Text)
import Prelude hiding (div, span)

-- | HTML ELements
-- see: https://developer.mozilla.org/en-US/docs/Web/HTML/Element

elmWithContent :: Text -> Text -> Elem cb ()
elmWithContent t c = elm t & E.content .~ c

address :: Elem cb ()
address = elm "address"

article :: Elem cb ()
article = elm "article"

footer :: Elem cb ()
footer = elm "footer"

header :: Elem cb ()
header = elm "header"

h1 :: Text -> Elem cb ()
h1 = elmWithContent "h1"

h2 :: Text -> Elem cb ()
h2 = elmWithContent "h2"

h3 :: Text -> Elem cb ()
h3 = elmWithContent "h3"

h4 :: Text -> Elem cb ()
h4 = elmWithContent "h4"

h5 :: Text -> Elem cb ()
h5 = elmWithContent "h5"

h6 :: Text -> Elem cb ()
h6 = elmWithContent "h6"

hgroup :: Elem cb ()
hgroup = elm "hgroup"

nav :: Elem cb ()
nav = elm "nav"

section :: Elem cb ()
section = elm "section"

dd :: Elem cb ()
dd = elm "dd"

div :: Elem cb ()
div = elm "div"

dl :: Elem cb ()
dl = elm "dl"

dt :: Elem cb ()
dt = elm "dt"

figcaption :: Elem cb ()
figcaption = elm "figcaption"

figure :: Elem cb ()
figure = elm "figure"

hr :: Elem cb ()
hr = elm "hr"

li :: Elem cb ()
li = elm "li"

main :: Elem cb ()
main = elm "main"

ol :: Elem cb ()
ol = elm "ol"

p :: Elem cb ()
p = elm "p"

pre :: Elem cb ()
pre = elm "pre"

ul :: Elem cb ()
ul = elm "ul"

abbr :: Elem cb ()
abbr = elm "abbr"

b :: Elem cb ()
b = elm "b"

bdi :: Elem cb ()
bdi = elm "bdi"

bdo :: Elem cb ()
bdo = elm "bdo"

br :: Elem cb ()
br = elm "br"

cite :: Elem cb ()
cite = elm "cite"

code :: Elem cb ()
code = elm "code"

data_ :: Elem cb ()
data_ = elm "data"

dfn :: Elem cb ()
dfn = elm "dfn"

em :: Elem cb ()
em = elm "em"

i :: Elem cb ()
i = elm "i"

kbd :: Elem cb ()
kbd = elm "kbd"

mark :: Elem cb ()
mark = elm "mark"

q :: Elem cb ()
q = elm "q"

rp :: Elem cb ()
rp = elm "rb"

rt :: Elem cb ()
rt = elm "rt"

rtc :: Elem cb ()
rtc = elm "rtc"

ruby :: Elem cb ()
ruby = elm "ruby"

s :: Elem cb ()
s = elm "s"

samp :: Elem cb ()
samp = elm "samp"

small :: Elem cb ()
small = elm "small"

span :: Elem cb ()
span = elm "span"

strong :: Elem cb ()
strong = elm "strong"

sub :: Elem cb ()
sub = elm "sub"

sup :: Elem cb ()
sup = elm "sup"

time :: Elem cb ()
time = elm "time"

u :: Elem cb ()
u = elm "u"

var_ :: Elem cb ()
var_ = elm "var"

wbr :: Elem cb ()
wbr = elm "wbr"

area :: Elem cb ()
area = elm "area"

audio :: Elem cb ()
audio = elm "audio"

map_ :: Elem cb ()
map_ = elm "map"

track :: Elem cb ()
track = elm "track"

video :: Elem cb ()
video = elm "video"

embed :: Elem cb ()
embed = elm "embed"

object :: Elem cb ()
object = elm "object"

param :: Elem cb ()
param = elm "param"

source :: Elem cb ()
source = elm "source"

canvas :: Elem cb ()
canvas = elm "canvas"

noscript :: Elem cb ()
noscript = elm "noscript"

script :: Elem cb ()
script = elm "script"

del :: Elem cb ()
del = elm "del"

ins :: Elem cb ()
ins = elm "ins"

caption :: Elem cb ()
caption = elm "caption"

col :: Elem cb ()
col = elm "col"

colgroup :: Elem cb ()
colgroup = elm "colgroup"

table :: Elem cb ()
table = elm "table"

tbody :: Elem cb ()
tbody = elm "tbody"

td :: Elem cb ()
td = elm "td"

tfoot :: Elem cb ()
tfoot = elm "tfoot"

th :: Elem cb ()
th = elm "th"

thead :: Elem cb ()
thead = elm "thead"

tr :: Elem cb ()
tr = elm "tr"

button :: Elem cb ()
button = elm "button"

datalist :: Elem cb ()
datalist = elm "datalist"

fieldset :: Elem cb ()
fieldset = elm "fieldset"

form :: Elem cb ()
form = elm "form"

input :: Elem cb ()
input = elm "input"

keygen :: Elem cb ()
keygen = elm "keygen"

label :: Elem cb ()
label = elm "label"

legend :: Elem cb ()
legend = elm "legend"

meter :: Elem cb ()
meter = elm "meter"

optgroup :: Elem cb ()
optgroup = elm "optgroup"

option :: Elem cb ()
option = elm "option"

output :: Elem cb ()
output = elm "output"

progress :: Elem cb ()
progress = elm "progress"

select :: Elem cb ()
select = elm "select"

details :: Elem cb ()
details = elm "details"

dialog :: Elem cb ()
dialog = elm "dialog"

menu :: Elem cb ()
menu = elm "menu"

menuitem :: Elem cb ()
menuitem = elm "menuitem"

summary :: Elem cb ()
summary = elm "summary"

content :: Elem cb ()
content = elm "content"

element :: Elem cb ()
element = elm "element"

shadow :: Elem cb ()
shadow = elm "shadow"

template :: Elem cb ()
template = elm "template"
