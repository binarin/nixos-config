Set-Location Registry::HKEY_CLASSES_ROOT
New-Item -Path .\org-protocol -Value 'URL:Org Protocol'
New-ItemProperty -Path .\org-protocol -Name 'URL Protocol' -Value ''
New-Item -Path .\org-protocol\shell
New-Item -Path .\org-protocol\shell\open
New-Item -Path .\org-protocol\shell\open\command -Value '"C:\Windows\System32\wsl.exe" emacsclient "%1"'
