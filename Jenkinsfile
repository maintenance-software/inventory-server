pipeline {
    agent any

    stages {
        stage('CleanOldBinary') {
            steps {
               sh 'rm -rf .stack-work'
            }
        }
        stage('Build') {
            steps {
                sh 'stack build'
                sh 'stack build --copy-bins --local-bin-path target'
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
