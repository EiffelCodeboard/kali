# Kali
Web service for compiling, running and testing programs. Part of the codeboard.io project.

_Important_: this project is deprecated and will no longer be developed. While still being used by the current version of Codeboard, future versions of Codeboard and Mantra will supersede the Kali project.


### Requirements ###

Kali requires NodeJS and Docker.

* Nodejs: tested with version 0.12.9
* Docker: tested with version 1.8.1 (installation instructions here: https://docs.docker.com/engine/installation/ubuntulinux/)
* Kali has been tested on an Ubuntu 14.04 system.

### Server setup ###

Before installing Kali, make sure you have NodeJS and Docker installed and you've configured your server using the following steps:

```
# add a group with name "cobo" and group id 2506
groupadd --gid 2506 cobo

# add a user "cobo" with user id 2506, the default group with gid 2506
# make sure the user is also in group "sudo" (this allows e.g. for ssh access for "cobo")
# create and set the home directory to /home/cobo  
useradd --uid 2506 --gid 2506 -G sudo -m -d /home/cobo -c "Codeboard user" cobo

# set the password for user cobo
passwd cobo

# add the cobo user to docker group (so it can run the docker cli)
usermod -a -G docker cobo

# create folders where we store Kali and the temporary files Kali will create
mkdir -p /var/www
mkdir -p /tmp/projects

# make cobo the owner of the relevant folders for Kali
chown -R cobo:cobo /var/www
chown -R cobo:cobo /tmp/projects

# reboot the machine (this seems to be necessary for Docker to work correctly)
shutdown -r now
```

### Installing Kali ###

1. Make sure you've completed the previous step "Server setup"
2. Make sure you're logged in as user ```cobo```
3. Clone this repository into /var/www
```
cd /var/www
git clone https://github.com/codeboardio/kali.git
```
4. Clone the open-codeboard-docker repository into /var/www
```
cd /var/www
git clone https://github.com/codeboardio/codeboard_docker.git
```
5. Make /var/www/scripts available in the path
```
# use the following command to append to the existing ~/.bashrc
printf "\nexport PATH=\$PATH:/var/www/kali/scripts\n" >> ~/.bashrc
```

5. Change into the kali folder and run ```npm install```
6. Change into the codeboard_docker folder and run the following commands
```
docker build --file="docker_build_files/cobo_ubuntu.docker" --tag="cobo/ubuntu" --rm=true .
docker build --file="docker_build_files/haskell-hspec.docker" --tag="cobo/haskell-hspec" --rm=true .
docker build --file="docker_build_files/java_and_java-junit.docker" --tag="cobo/java8-junit4" --rm=true .
docker build --file="docker_build_files/py_and_py-unittest.docker" --tag="cobo/python" --rm=true .
```

### Running and Testing Kali ###

To run Kali (using settings file env/production.js):

```
npm run-script deploy
```

To test Kali (using settings file env/production.js):

```
npm run-script testProduction
```


### Cron jobs (optional) ####
The ```scripts``` folder contains two bash script that can be
run as Cron jobs when deploying Kali. The scripts are only needed to handle cases where Kali would crash and thus not
clean up created containers or folders. The use of these scripts is optional but recommended:

* ```cron_rm_stopped_dockers.sh```: this script will remove
all the Docker containers that were created (to compile or run
programs) and that have exited since (usually Kali does this automatically). You could run this
script every 15 minutes.

* ```cron_rm_folders.sh```: this script will remove all sub-folders in ```/tmp/projects``` which are older than 60 minutes.
By default, Kali stores users' files in ```/tmp/projects``` but removes these files automatically. Should there be any
folders older than 60 minutes in /tmp/projects, it's unlikely they should still be there. You could run this script every 15 minutes.

### Licensing ###
Kali is available under the MIT license. See LICENSE for the full license text.

_Important_: Kali uses 3rd party software that may use others licences. If you're planning to use Kali, make sure
your use case complies with all 3rd party licenses.
