signatureVerdict <-
  function(signature_name,
           verdict,
           userHandle,
           thisUser) {
    thisPath = "sigrepo.bu.edu:3838/signatures/queue/" #needs to be sys.getenv...
    #should I force verdict to be only Boolean typed?
    if (verdict) {
      addSignature(paste0(thisPath, signature_name, "_obj.json"),
                          verdict,
                          userHandle,
                          thisUser)
    }
    else{
      junk <- dir(path = thisPath,
                  pattern = signature_name)
      file.remove(junk)
      sqlGeneric(paste0(
        "DELETE
        FROM
          signature_queue
        WHERE
          signature_name=",
        singleQuote(signature_name),
        ";"
      ))
    }
  }