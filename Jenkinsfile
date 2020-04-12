pipeline {
    agent any

    stages {
        stage('CleanOldBinary') {
            steps {
               catchError {
                 sh 'rm -rf webapps/dist'
//               sh 'rm -rf .stack-work'
                 sh 'docker stop inventory-server'
                 sh 'docker rm inventory-server'
                 sh 'docker images -a | grep "inventory-server" | awk \'{print $3}\' | xargs docker rmi'
               }
            }
        }
        stage('BuildUI') {
          steps {
              sh 'mkdir webapps/dist'
              sh 'git submodule update --init'
              dir("ui-home") {
                  sh 'yarn install && yarn build'
              }
              sh 'cp -a ui-home/build/. webapps/dist'
          }
        }
        stage('Build') {
            steps {
                sh 'stack build --copy-bins --local-bin-path target'
            }
        }
        stage('DockerBuildImage') {
            steps {
                echo 'Starting to build docker image'
                script {
                    def customImage = docker.build("inventory-server:1.0")
                }
            }
        }
        stage('Test') {
            steps {
                echo 'Testing..'
            }
        }
        stage('Deploy') {
            steps {
                echo 'Deploying....'
                script {
                    docker.image("inventory-server:1.0")
                    .run('--name inventory-server --net=host '
                        + '-e YESOD_PORT=3000 '
                        + '-e YESOD_PGUSER=inventory_user '
                        + '-e YESOD_PGPASS=inventory_password '
                        + '-e YESOD_PGHOST=192.168.0.107 '
                        + '-e OAUTH2_CLIENT_ID=app '
                        + '-e OAUTH2_SECRET=appsecret '
                        + '-e OAUTH2_AUTHORIZE=http://192.168.0.107:4200/oauth/authorize '
                        + '-e OAUTH2_ACCESS_TOKEN=http://192.168.0.107:4200/oauth/token '
                        + '-e OAUTH2_USER_INFO=http://192.168.0.107:4200/connect/userinfo '
                        + '-e OAUTH2_LOGOUT=http://192.168.0.107:4200/logout '
                        + '-e OAUTH2_SCOPES=openid'
                    )
                }
            }
        }
    }
}
