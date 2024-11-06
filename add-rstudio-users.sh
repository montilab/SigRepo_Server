#!/bin/bash
# Wrapper to add users 


# Add Cameron
adduser cameron --gecos 'Cameron Vicnaire' --disabled-password

sh -c 'echo cameron:password123 | sudo chpasswd'

adduser cameron staff


# Add Stefano

adduser stefano --gecos 'Stefano Monti' --disabled-password

sh -c 'echo stefano:password123 | sudo chpasswd'

adduser stefano staff


# Add Vanessa

adduser vanessa --gecos 'Vanessa Mengze Li' --disabled-password

sh -c 'echo vanessa:password123 | sudo chpasswd'

adduser vanessa staff


