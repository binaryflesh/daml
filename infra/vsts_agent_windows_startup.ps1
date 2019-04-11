#!/usr/bin/env pwsh
# Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0


# Install chocolatey
iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))

# Add-LocalGroupMember -Group "Administrators" -Member "vsts"

## Install runtime dependencies

choco install powershell-core --yes
choco install git --yes --params "'/GitAndUnixToolsOnPath /NoShellIntegration'"

# Install scoop
iex (new-object net.webclient).downloadstring('https://get.scoop.sh')

# Create the VSTS user
New-LocalUser "vsts" -NoPassword -FullName "VSTS agent"

## Install the VSTS agent
choco install azure-pipelines-agent --yes --params "'/Token:${vsts_token} /Pool:${vsts_pool} /Url:https://${vsts_account}.visualstudio.com'"
# TODO: /LogonAccount:.\vsts'"
