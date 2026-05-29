let strf = Printf.sprintf

let bash_generic_completion fun_name = strf
{|%s() {
  local words cword
  # Equivalent of COMP_WORDS, COMP_CWORD but allow us to  exclude '=' as a word separator
  _get_comp_words_by_ref -n = words cword

  local prefix="${words[cword]}"
  local w=("${words[@]}") # Keep words intact for restart completion
  w[cword]="--__complete=${words[cword]}"
  local line="${w[@]:0:1} --__complete ${w[@]:1}"
  local version type group item text_line item_doc msg
  {
    read version
    if [[ $version != "1" ]]; then
      printf "\nUnsupported cmdliner completion protocol: $version" >&2
      return 1
    fi
    while read type; do
      if [[ $type == "group" ]]; then
        read group
      elif [[ $type == "dirs" || $type == "files" ]] && (type compopt &> /dev/null); then
        # trim option prefix in cases like --file=<TAB> or -f<TAB>
        local pattern="$prefix"
        local reply_prefix=""
        if [[ $pattern == --* ]]; then
          pattern="${prefix#*=}"
        elif [[ $pattern == -* ]]; then
          pattern="${prefix:2}"
          reply_prefix="${prefix:0:2}"
        fi

        # enable filename completion features like trailing slash for dirs
        compopt -o filenames -o nospace

        # need to run compgen with -d or -f flag
        local flag="${type:0:1}"
        local completions=( $(compgen -$flag "$pattern") )
        for c in "${completions[@]}"; do
          COMPREPLY+=("${reply_prefix}${c}")
        done
      elif [[ $type == "message" ]]; then
          msg="";
          while read text_line; do
              if [[ "$text_line" == "message-end" ]]; then
                  msg=${msg#?} # remove first newline
                  break
              fi
              msg+=$'\n'"$text_line"
          done
          printf "$msg" >&2
      elif [[ $type == "item" ]]; then
        read item;
        item_doc="";
        while read text_line; do
            if [[ "$text_line" == "item-end" ]]; then
                item_doc=${item_doc#?} # remove first newline
                break
            fi
            item_doc+=$'\n'"$text_line"
        done
        # Sadly it seems bash does not support doc strings, so we only
        # add item to to the reply. If you know any better get in touch.
        if [[ $group == "Values" ]] && [[ $prefix == -* ]] && [[ $prefix != --* ]]; then
          # properly complete short options
          item="${prefix:0:2}$item"
        fi
        COMPREPLY+=($item)
      elif [[ $type == "restart" ]]; then
          # N.B. only emitted if there is a -- token
          for ((i = 0; i < ${#words[@]}; i++)); do
              if [[ "${words[i]}" == "--" ]]; then
                  _command_offset $((i+1))
                  return
              fi
          done
      fi
    done } < <(eval $line)
  return 0
}
|} fun_name

let zsh_generic_completion fun_name = strf
{|function %s {
  local w=("${words[@]}") # Keep words intact for restart completion
  local prefix="${words[CURRENT]}"
  w[CURRENT]="--__complete=${words[CURRENT]}"
  local line="${w[@]:0:1} --__complete ${w[@]:1}"
  local -a completions
  local version type group item text_line item_doc msg
  eval $line | {
    read -r version
    if [[ $version != "1" ]]; then
      _message -r "Unsupported cmdliner completion protocol: $version"
      return 1
    fi
    while IFS= read -r type; do
      if [[ "$type" == "group" ]]; then
        if [ -n "$completions" ]; then
          _describe -V unsorted completions -U
          completions=()
        fi
        read -r group
      elif [[ "$type" == "message" ]]; then
          msg="";
          while read text_line; do
              if [[ "$text_line" == "message-end" ]]; then
                  msg=${msg#?} # remove first newline
                  break
              fi
              msg+=$'\n'"$text_line"
          done
          _message -r "$msg"
      elif [[ "$type" == "item" ]]; then
        read -r item;
        item_doc="";
        while read -r text_line; do
            if [[ "$text_line" == "item-end" ]]; then
                item_doc=${item_doc#?} # remove first space
                break
            fi
            # Sadly it seems impossible to make multiline
            # doc strings. Get in touch if you know any better.
            item_doc+=" $text_line"
        done
        # Handle glued forms, the completion item is the full option
        if [[ "$group" == "Values" ]]; then
            if [[ "$prefix" == --* ]]; then
                item="${prefix%%=*}=${item}"
            elif [[ "$prefix" == -* ]]; then
                item="${prefix:0:2}${item}"
            fi
        fi
        item_doc="${item_doc//$'\e'\[(01m|04m|m)/}"
        completions+=("${item}":"${item_doc}")
      elif [[ "$type" == "dirs" || "$type" == "files" ]]; then
          local pre=""
          local pat="$prefix"
          if [[ "$prefix" == --* ]]; then
              pre="${prefix%%=*}="
              pat="${prefix#*=}"
          elif [[ "$prefix" == -* ]]; then
              pre="${prefix:0:2}"
              pat="${prefix:2}"
          fi
          if [[ "$type" == "dirs" ]]; then
              _path_files -/ -P "$pre" "$pat"
          else
              _path_files -f -P "$pre" "$pat"
          fi
      elif [[ "$type" == "restart" ]]; then
        # N.B. only emitted if there is a -- token
        while [[ $words[1] != "--" ]]; do
          shift words
          (( CURRENT-- ))
        done
        shift words
        (( CURRENT-- ))
        _normal
      fi
    done
  }
  if [ -n "$completions" ]; then
    _describe -V unsorted completions -U
  fi
}
|} fun_name

let pwsh_generic_completion fun_name = strf
{|<#
Note: PowerShell swallows all errors in tab completion functions.
If you are hacking on this file and get unexpected results (like
file completions where you don't expect them), inspect the most
recent errors with `Get-Error -Newest N` for some small N.
#>

$Global:%s = {
  param(
    $wordToComplete,
    $commandAst,
    $cursorPosition
  )

  $exe = $commandAst.CommandElements[0].Extent.Text
  $otherArgs = ""

  $seenWordToComplete = $false
  foreach ($elem in $commandAst.CommandElements | Select-Object -Skip 1) {
    $text = $elem.Extent.Text
    if ($otherArgs -ne "") {
      $otherArgs += " "
    }
    # wordToComplete has had quoting removed, so we need to remove it before comparison
    if ($text -replace "[`"']", "" -eq $wordToComplete) {
      $otherArgs += "--__complete=$text"
      $seenWordToComplete = $true
    }
    else {
      $otherArgs += $text
    }
  }
  # usually if wordToComplete is not in commandAst, it's because it's an empty string
  if (-not $seenWordToComplete) {
    $otherArgs += " --__complete=$wordToComplete"
  }

  $completions = Invoke-Expression "$exe --__complete $otherArgs"

  $version = $completions[0]
  if ($version -ne "1") {
    throw "Unsupported cmdliner completion protocol: $version"
  }

  $prefix = ""
  $pattern = $wordToComplete
  if ($wordToComplete -match '^\-\-') {
    # take everything before '=' as prefix for long options
    $parts = $wordToComplete -split '=', 2
    $prefix = $parts[0] + '='
    $pattern = $parts[1]
  }
  elseif ($wordToComplete -match '^\-') {
    # take first two characters of wordToComplete as prefix for short options
    $prefix = $wordToComplete.Substring(0, [math]::Min(2, $wordToComplete.Length))
    $pattern = $wordToComplete.Substring(2)
  }

  $CompletionResults = [System.Collections.Generic.List[System.Management.Automation.CompletionResult]]::new()

  $idx = 1
  $group = ""
  while ($idx -lt $completions.Count) {
    $type = $completions[$idx]
    $idx += 1

    switch ($type) {
      "group" {
        $group = $completions[$idx]
        $idx += 1
      }
      "item" {
        $item = $completions[$idx]
        $idx += 1

        $itemDoc = ""
        while ($true) {
          $line = $completions[$idx]
          $idx += 1
          if ($line -eq "item-end") {
            break
          }
          if ($itemDoc -ne "") {
            $itemDoc += "`n"
          }
          $itemDoc += $line
        }
        # avoid null tooltip error
        if ($itemDoc -eq "") {
          $itemDoc = $item
        }

        $completionItem = $item

        if ($group -eq "Values") {
          # quote replies with powershell separators
          if ($completionItem -match "[,|;]") {
            $completionItem = '"' + $completionItem + '"'
          }
          # re-add prefix for things like --foo=
          $completionItem = $prefix + $completionItem
        }

        $CompletionResults.Add(
          [System.Management.Automation.CompletionResult]::new(
            $completionItem,
            $item,
            'ParameterValue',
            $itemDoc))
      }
      "files" {
        [Management.Automation.CompletionCompleters]::CompleteFilename(
          $pattern
        ) | ForEach-Object {
          $CompletionResults.Add(
            [System.Management.Automation.CompletionResult]::new(
              $prefix + $_.CompletionText,
              $_.ListItemText,
              $_.ResultType,
              $_.ToolTip))
        }
      }
      "dirs" {
        [Management.Automation.CompletionCompleters]::CompleteFilename(
          $pattern
        ) | Where-Object {
          Test-Path $_.CompletionText -PathType Container
        } | ForEach-Object {
          $CompletionResults.Add(
            [System.Management.Automation.CompletionResult]::new(
              $prefix + $_.CompletionText,
              $_.ListItemText,
              $_.ResultType,
              $_.ToolTip))
        }
      }
      "restart" {
        $newCommand = ""
        $seenDoubleDash = $false

        $newCursorPosition = 0
        for ($i = 1; $i -lt $commandAst.CommandElements.Count; $i++) {
          $elem = $commandAst.CommandElements[$i]
          if ($seenDoubleDash) {
            if ($newCommand -ne "") {
              $newCommand += " "
            }
            $newCommand += $elem.Extent.Text
            if ($elem.Extent.Text -eq $wordToComplete) {
              $newCursorPosition = $newCommand.Length
            }
          }
          if ($elem.Extent.Text -eq "--") {
            $seenDoubleDash = $true
          }
        }

        if ($newCursorPosition -eq 0) {
          $newCommand += " "
          $newCursorPosition = $newCommand.Length
        }

        $newCommandCompletions = TabExpansion2 -inputScript $newCommand -cursorColumn $newCursorPosition

        $newCommandCompletions.CompletionMatches | ForEach-Object {
          $CompletionResults.Add($_)
        }
      }
      "message" {
        $msg = ""
        while ($true) {
          $line = $completions[$idx]
          $idx += 1
          if ($line -eq "message-end") {
            break
          }
          if ($msg -ne "") {
            $msg += "`n"
          }
          $msg += $line
        }
        Write-Output "$msg" | Out-Host
      }
      default {
        throw "Unknown completion type: $type"
      }
    }
  }

  if ($CompletionResults.Count -gt 0) {
    return $CompletionResults
  }
  else {
    # supress default behavior (file completion) when there are no completions
    return $null
  }
}
|} fun_name