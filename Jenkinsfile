pipeline {
    agent any

    stages {
        stage('CleanOldBinary') {
            steps {
               catchError {
//                 sh 'rm -rf webapps/dist'
//               sh 'rm -rf .stack-work'
//                 sh 'docker stop inventory-server'
//                 sh 'docker rm inventory-server'
//                 sh 'docker images -a | grep "inventory-server" | awk \'{print $3}\' | xargs docker rmi'
               }
            }
        }
        stage('Build') {
            steps {
//                sh 'mkdir webapps/dist'
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
                    docker.image("inventory-server:1.0").withRun(
                        '--name inventory-server'
                        , '--net=host'
                        + '-e YESOD_PORT=3000'
                        + '-e YESOD_PGUSER=inventory_user'
                        + '-e YESOD_PGPASS=inventory_password'
                        + '-e YESOD_PGHOST=192.168.0.107'
                        + '-d'
                    )
                }
            }
        }
    }
}
