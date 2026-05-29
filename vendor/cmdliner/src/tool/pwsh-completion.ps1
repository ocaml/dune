<#
Note: PowerShell swallows all errors in tab completion functions.
If you are hacking on this file and get unexpected results (like
file completions where you don't expect them), inspect the most
recent errors with `Get-Error -Newest N` for some small N.
#>

$Global:_cmdliner_generic = {
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
