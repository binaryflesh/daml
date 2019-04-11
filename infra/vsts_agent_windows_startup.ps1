#!/usr/bin/env pwsh
# Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

Set-StrictMode -Version latest
$ErrorActionPreference = 'Stop'

# Install scoop
# iex (New-Object System.Net.WebClient).DownloadString('https://get.scoop.sh')

# Install chocolatey
iex (New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1')

## Install runtime dependencies

choco feature enable -n=allowGlobalConfirmation

#choco install git.install --yes --params "/GitAndUnixToolsOnPath /NoShellIntegration /NoGitLfs /SChannel"
#choco install msys2 --yes
#choco install powershell-core --yes
choco install git.portable --force --debug --yes

# Update the PATH with the new tools
$oldpath = (Get-ItemProperty -Path ‘Registry::HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment’ -Name PATH).path
$newpath = "$oldpath;C:\tools\git\bin"
Set-ItemProperty -Path ‘Registry::HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment’ -Name PATH -Value $newPath

# Create the VSTS user
New-LocalUser "vsts" -NoPassword -FullName "VSTS agent"
# Add-LocalGroupMember -Group "Administrators" -Member "vsts"

## Install the VSTS agent
choco install azure-pipelines-agent --yes --params "'/Token:${vsts_token} /Pool:${vsts_pool} /Url:https://${vsts_account}.visualstudio.com'"
# /LogonAccount:$env:COMPUTERNAME\\vsts'"
