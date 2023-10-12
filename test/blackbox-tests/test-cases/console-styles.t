We test the different styles that dune_console can output. This is done by running a
special binary that can be run separately for manual inspection. It will simply dump the
ansi codes used here so that any changes to how styles are rendered can be easily seen.

Trick dune into thinking this is a color terminal:
  $ CLICOLOR_FORCE=1
  > dune_console_styles 2> output

To get around the cram sanitation, we must use tr to replace the escape codes with a
character that cram will accept. We also only show the first 62 lines as what follows are
color tables that are too verbose to include here.
  $ < output tr '\033' '?' | head -n 62
  stderr supports color: true
  
  ?[1mDune Console User_message Styles:?[0m
  - ?[1mLoc?[0m
  - ?[1;31mError?[0m
  - ?[1;35mWarning?[0m
  - ?[1;34mKwd?[0m
  - ?[1;33mId?[0m
  - ?[1;32mPrompt?[0m
  - ?[3;37mHint?[0m
  - ?[2;37mDetails?[0m
  - ?[2;32mOk?[0m
  - ?[4;96mDebug?[0m
  - ?[1;32mSuccess?[0m
  
  ?[1mANSI Terminal Colors and Styles:?[0m
  - ?[30mFg_black?[0m
  - ?[31mFg_red?[0m
  - ?[32mFg_green?[0m
  - ?[33mFg_yellow?[0m
  - ?[34mFg_blue?[0m
  - ?[35mFg_magenta?[0m
  - ?[36mFg_cyan?[0m
  - ?[37mFg_white?[0m
  - ?[90mFg_bright_black?[0m
  - ?[91mFg_bright_red?[0m
  - ?[92mFg_bright_green?[0m
  - ?[93mFg_bright_yellow?[0m
  - ?[94mFg_bright_blue?[0m
  - ?[95mFg_bright_magenta?[0m
  - ?[96mFg_bright_cyan?[0m
  - ?[97mFg_bright_white?[0m
  - ?[49mBg_default?[0m
  - ?[40mBg_black?[0m
  - ?[41mBg_red?[0m
  - ?[42mBg_green?[0m
  - ?[43mBg_yellow?[0m
  - ?[44mBg_blue?[0m
  - ?[45mBg_magenta?[0m
  - ?[46mBg_cyan?[0m
  - ?[47mBg_white?[0m
  - ?[100mBg_bright_black?[0m
  - ?[101mBg_bright_red?[0m
  - ?[102mBg_bright_green?[0m
  - ?[103mBg_bright_yellow?[0m
  - ?[104mBg_bright_blue?[0m
  - ?[105mBg_bright_magenta?[0m
  - ?[106mBg_bright_cyan?[0m
  - ?[107mBg_bright_white?[0m
  - ?[1mBold?[0m
  - ?[2mDim?[0m
  - ?[3mItalic?[0m
  - ?[4mUnderline?[0m
  
  ?[1mExamples of ANSI 256-colors:?[0m
  - ?[38;5;0m  0?[0m ?[38;5;32m 32?[0m ?[38;5;96m 96?[0m ?[38;5;160m160?[0m ?[38;5;255m255?[0m 
  - ?[38;5;0m  0?[0m ?[38;5;32m 32?[0m ?[38;5;96m 96?[0m ?[38;5;160m160?[0m ?[38;5;255m255?[0m 
  
  ?[1mExamples of ANSI 24-colors:?[0m
  - ?[38;2;0;0;0m ?[0m ?[38;2;128;0;0m ?[0m ?[38;2;0;128;0m ?[0m ?[38;2;128;128;128m ?[0m 
  - ?[38;2;0;0;0m ?[0m ?[38;2;128;0;0m ?[0m ?[38;2;0;128;0m ?[0m ?[38;2;128;128;128m ?[0m 
  
