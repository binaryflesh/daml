# Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

data "template_file" "vsts-agent-windows-startup" {
  template = "${file("${path.module}/vsts_agent_windows_startup.ps1")}"

  vars = {
    vsts_token   = "${secret_resource.vsts-token.value}"
    vsts_account = "digitalasset"
    vsts_pool    = "windows-pool"
  }
}

resource "google_compute_region_instance_group_manager" "vsts-agent-windows" {
  provider           = "google-beta"
  name               = "vsts-agent-windows"
  # keep the name short. windows hostnames are limited to 12(?) chars.
  # -5 for the random postfix:
  base_instance_name = "vsts-win"
  region             = "${local.region}"
  target_size        = 1

  version {
    name              = "vsts-agent-windows"
    instance_template = "${google_compute_instance_template.vsts-agent-windows.self_link}"
  }

  update_policy {
    type            = "PROACTIVE"
    minimal_action  = "REPLACE"
    max_surge_fixed = 3
    min_ready_sec   = 60
  }
}

resource "google_compute_instance_template" "vsts-agent-windows" {
  name_prefix  = "vsts-agent-windows-"
  machine_type = "n1-standard-8"
  labels       = "${local.labels}"

  disk {
    disk_size_gb = 100
    disk_type    = "pd-ssd"

    # find the image name with `gcloud compute images list`
    source_image = "windows-cloud/windows-2016-core"
  }

  lifecycle {
    create_before_destroy = true
  }

  metadata {
    windows-startup-script-ps1  = "${data.template_file.vsts-agent-windows-startup.rendered}"
    windows-shutdown-script-ps1 = "c://agent/config remove --unattended --auth PAT --token '${secret_resource.vsts-token.value}'"
  }

  network_interface {
    network = "default"

    // Ephemeral IP to get access to the Internet
    access_config {}
  }

  scheduling {
    automatic_restart   = false
    on_host_maintenance = "TERMINATE"
    preemptible         = true
  }
}
