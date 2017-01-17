node {
  repo = 'stronghold-ui'
  tag = env.BUILD_TAG

  wrap([$class: 'TimestamperBuildWrapper']) {
    try {
      // Mark the code checkout 'stage'....
      stage 'Checkout'

      // Checkout code from repository
      checkout scm
      sh 'git submodule update --init'

      // Mark the code build 'stage'....
      stage 'Build'

      // Run cide
      sh "cide exec -t '${tag}'"

      stage 'Package'

      sh "git log -1 --format=%cd-${env.BUILD_NUMBER}-%h --date=short > .git/build-id"
      build_id = readFile('.git/build-id').trim() + "-${env.BRANCH_NAME}"
      package_file = ".package/${repo}.${build_id}.tar.gz"
      sh "cide package --no-upload -t '${tag}' --package '${repo}' --build-id '${build_id}'"

      stage 'Archive'

      archive package_file

      step([
        $class: 'S3BucketPublisher', profileName: 'packager', entries: [[
          bucket: 'pusher_packager',
          sourceFile: package_file,
          flatten: true,
          noUploadOnFailure: true,
        ]]
      ])
    } catch (err) {
      slackSend(
        color: 'danger',
        message: "${env.JOB_NAME}@${env.BUILD_NUMBER} build failed !! OMG. ${err} (<${env.BUILD_URL}|Open>)",
        channel: "#engineering-team",
      )
      throw err
    }
  }
}
