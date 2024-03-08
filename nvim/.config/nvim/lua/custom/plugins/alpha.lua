return {
  'goolord/alpha-nvim',
  config = function()
    local dashboard = require('alpha.themes.dashboard')
    dashboard.section.header.val = {
      "            :h-                                  Nhy`               ",
      "           -mh.                           h.    `Ndho               ",
      "           hmh+                          oNm.   oNdhh               ",
      "          `Nmhd`                        /NNmd  /NNhhd               ",
      "          -NNhhy                      `hMNmmm`+NNdhhh               ",
      "          .NNmhhs              ```....`..-:/./mNdhhh+               ",
      "           mNNdhhh-     `.-::///+++////++//:--.`-/sd`               ",
      "           oNNNdhhdo..://++//++++++/+++//++///++/-.`                ",
      "      y.   `mNNNmhhhdy+/++++//+/////++//+++///++////-` `/oos:       ",
      " .    Nmy:  :NNNNmhhhhdy+/++/+++///:.....--:////+++///:.`:s+        ",
      " h-   dNmNmy oNNNNNdhhhhy:/+/+++/-         ---:/+++//++//.`         ",
      " hd+` -NNNy`./dNNNNNhhhh+-://///    -+oo:`  ::-:+////++///:`        ",
      " /Nmhs+oss-:++/dNNNmhho:--::///    /mmmmmo  ../-///++///////.       ",
      "  oNNdhhhhhhhs//osso/:---:::///    /yyyyso  ..o+-//////////:/.      ",
      "   /mNNNmdhhhh/://+///::://////     -:::- ..+sy+:////////::/:/.     ",
      "     /hNNNdhhs--:/+++////++/////.      ..-/yhhs-/////////::/::/`    ",
      "       .ooo+/-::::/+///////++++//-/ossyyhhhhs/:///////:::/::::/:    ",
      "       -///:::::::////++///+++/////:/+ooo+/::///////.::://::---+`   ",
      "       /////+//++++/////+////-..//////////::-:::--`.:///:---:::/:   ",
      "       //+++//++++++////+++///::--                 .::::-------::   ",
      "       :/++++///////////++++//////.                -:/:----::../-   ",
      "       -/++++//++///+//////////////               .::::---:::-.+`   ",
      "       `////////////////////////////:.            --::-----...-/    ",
      "        -///://////////////////////::::-..      :-:-:-..-::.`.+`    ",
      "         :/://///:///::://::://::::::/:::::::-:---::-.-....``/- -   ",
      "           ::::://::://::::::::::::::----------..-:....`.../- -+oo/ ",
      "            -/:::-:::::---://:-::-::::----::---.-.......`-/.      ``",
      "           s-`::--:::------:////----:---.-:::...-.....`./:          ",
      "          yMNy.`::-.--::..-dmmhhhs-..-.-.......`.....-/:`           ",
      "         oMNNNh. `-::--...:NNNdhhh/.--.`..``.......:/-              ",
      "        :dy+:`      .-::-..NNNhhd+``..`...````.-::-`                ",
      "                        .-:mNdhh:.......--::::-`                    ",
      "                           yNh/..------..`                          ",
      "                                                                    ",
    }
    require 'alpha'.setup(dashboard.opts)
  end
};