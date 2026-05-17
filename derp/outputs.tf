output "derper_ip" {
  description = "Elastic IP — point your DNS hostname at this."
  value       = aws_eip.derper.public_ip
}

output "ssh_command" {
  description = "SSH command to access the VM."
  value       = "ssh -i ${replace(var.ssh_public_key_path, ".pub", "")} ubuntu@${aws_eip.derper.public_ip}"
}

output "next_steps" {
  value = <<-EOT

    Next steps:

    1. Point ${var.derper_hostname} -> ${aws_eip.derper.public_ip}
       (DuckDNS: https://www.duckdns.org/  manual update — no cron needed, EIP is static)

    2. SSH in and watch cloud-init + derper come up (first boot ~2-3 min):
         ssh -i ${replace(var.ssh_public_key_path, ".pub", "")} ubuntu@${aws_eip.derper.public_ip}
         sudo journalctl -u derper -f          # follow derper logs
         sudo cloud-init status --wait          # wait for cloud-init to finish

    3. From your laptop, verify HTTPS reachability:
         curl https://${var.derper_hostname}/
       Expect: a derper banner page.

    4. Add to Tailscale ACL at https://login.tailscale.com/admin/acls

       {
         "derpMap": {
           "OmitDefaultRegions": false,
           "Regions": {
             "900": {
               "RegionID":   900,
               "RegionCode": "twn",
               "RegionName": "Taiwan (AWS self-hosted)",
               "Nodes": [{
                 "Name":     "twn-1",
                 "RegionID": 900,
                 "HostName": "${var.derper_hostname}"
               }]
             }
           }
         }
       }

    5. Verify from any Tailscale node:
         tailscale netcheck
         tailscale ping <peer>
  EOT
}
