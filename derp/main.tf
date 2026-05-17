terraform {
  required_version = ">= 1.5"
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}

provider "aws" {
  region  = var.region
  profile = var.aws_profile
}

data "aws_ami" "ubuntu" {
  most_recent = true
  owners      = ["099720109477"]

  filter {
    name   = "name"
    values = ["ubuntu/images/hvm-ssd-gp3/ubuntu-noble-24.04-amd64-server-*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }
}

resource "aws_vpc" "derper" {
  cidr_block           = "10.0.0.0/16"
  enable_dns_hostnames = true
  enable_dns_support   = true

  tags = { Name = "derper-vpc" }
}

resource "aws_internet_gateway" "derper" {
  vpc_id = aws_vpc.derper.id
  tags   = { Name = "derper-igw" }
}

resource "aws_subnet" "derper" {
  vpc_id                  = aws_vpc.derper.id
  cidr_block              = "10.0.1.0/24"
  availability_zone       = "${var.region}a"
  map_public_ip_on_launch = true

  tags = { Name = "derper-subnet" }
}

resource "aws_route_table" "derper" {
  vpc_id = aws_vpc.derper.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.derper.id
  }

  tags = { Name = "derper-rt" }
}

resource "aws_route_table_association" "derper" {
  subnet_id      = aws_subnet.derper.id
  route_table_id = aws_route_table.derper.id
}

resource "aws_security_group" "derper" {
  name        = "derper-sg"
  description = "Allow SSH, HTTP, HTTPS, STUN"
  vpc_id      = aws_vpc.derper.id

  ingress {
    description = "SSH"
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    description = "HTTP (Lets Encrypt ACME)"
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    description = "HTTPS / DERP"
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    description = "STUN"
    from_port   = 3478
    to_port     = 3478
    protocol    = "udp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = { Name = "derper-sg" }
}

resource "aws_key_pair" "derper" {
  key_name   = "derper-key"
  public_key = file(pathexpand(var.ssh_public_key_path))
}

resource "aws_eip" "derper" {
  domain = "vpc"
  tags   = { Name = "derper-eip" }
}

resource "aws_instance" "derper" {
  ami                    = data.aws_ami.ubuntu.id
  instance_type          = var.instance_type
  subnet_id              = aws_subnet.derper.id
  vpc_security_group_ids = [aws_security_group.derper.id]
  key_name               = aws_key_pair.derper.key_name

  user_data = templatefile("${path.module}/cloud-init.yaml", {
    hostname = var.derper_hostname
  })

  root_block_device {
    volume_size = 10
    volume_type = "gp3"
  }

  tags = { Name = "derper" }
}

resource "aws_eip_association" "derper" {
  instance_id   = aws_instance.derper.id
  allocation_id = aws_eip.derper.id
}

data "aws_route53_zone" "main" {
  name         = var.route53_zone
  private_zone = false
}

resource "aws_route53_record" "derper" {
  zone_id = data.aws_route53_zone.main.zone_id
  name    = var.derper_hostname
  type    = "A"
  ttl     = 300
  records = [aws_eip.derper.public_ip]
}
