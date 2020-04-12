pipeline {
    agent any

    stages {
        stage('CleanOldBinary') {
            steps {
               sh 'rm -rf webapps'
               sh 'rm -rf .stack-work'
            }
        }
        stage('Build') {
            steps {
                sh 'mkdir webapps/dist'
                sh 'stack build'
                sh 'stack build --copy-bins --local-bin-path target'
            }
        }
        stage('DockerBuildImage') {
                    steps {
                        sh 'docker build --rm --tag inventory-server:1.0 ./'
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
            }
        }
    }
}
