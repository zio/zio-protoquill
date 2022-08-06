#!/bin/bash

# From 'All In One' of Quill CONTRIBUTING.md
docker-compose down && docker-compose build && docker-compose run --rm --service-ports setup
