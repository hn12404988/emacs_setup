variable "region" {
  description = "AWS region"
  type        = string
  default     = "ap-east-2"
}

variable "aws_profile" {
  description = "AWS CLI profile to use"
  type        = string
  default     = "willy"
}

variable "instance_type" {
  description = "EC2 instance type. t3.micro is 2 vCPU (burst), 1GB RAM. derper itself uses <50MB but Go build needs >512MB."
  type        = string
  default     = "t3.micro"
}

variable "ssh_public_key_path" {
  description = "Path to local SSH public key to install on the instance"
  type        = string
  default     = "~/.ssh/id_ed25519.pub"
}

variable "derper_hostname" {
  description = "Public hostname for derper (e.g. derp.willyderp.click). Must be a subdomain of route53_zone."
  type        = string
}

variable "route53_zone" {
  description = "Route 53 hosted zone name (must already exist in account, e.g. willyderp.click). Created automatically when registering a domain via Route 53."
  type        = string
}
