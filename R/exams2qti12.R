## create IMS QTI 1.2 .xml files
## specifications and examples available at:
## http://www.imsglobal.org/question/qtiv1p2/imsqti_asi_bindv1p2.html
## http://www.imsglobal.org/question/qtiv1p2/imsqti_asi_bestv1p2.html#1466669
exams2qti12 <- function(file, n = 1L, nsamp = NULL, dir = ".",
  name = NULL, quiet = TRUE, edir = NULL, tdir = NULL, sdir = NULL, verbose = FALSE, rds = FALSE, seed = NULL,
  resolution = 100, width = 4, height = 4, svg = FALSE, encoding  = "UTF-8",
  num = NULL, mchoice = NULL, schoice = mchoice, string = NULL, cloze = NULL,
  template = "qti12",
  duration = NULL, stitle = "Exercise", ititle = "Question",
  adescription = "Please solve the following exercises.",
  sdescription = "Please answer the following question.",
  maxattempts = 1, cutvalue = 0, solutionswitch = TRUE, zip = TRUE,
  points = NULL, eval = list(partial = TRUE, rule = "false2", negative = FALSE),
  converter = NULL, envir = NULL, engine = NULL, xmlcollapse = FALSE,
  flavor = c("plain", "openolat", "canvas", "ilias"), ...)
{
  ## which qti flavor
  flavor <- match.arg(flavor, c("plain", "openolat", "canvas", "ilias"))

  ## Canvas?
  canvas <- flavor == "canvas"
  if(canvas) {
    if(!eval$partial | eval$negative)
      warning("the current supported evaluation policy for Canvas is partial = TRUE and negative = FALSE, will be overwritten!")
    eval <- list(partial = TRUE, rule = eval$rule, negative = FALSE)
  }

  if(flavor == "openolat") {
    if(is.null(converter)) converter <- "pandoc-mathjax"
    ## post-process mathjax output for display in OpenOlat
    .exams_set_internal(pandoc_mathjax_fixup = "openolat")
    on.exit(.exams_set_internal(pandoc_mathjax_fixup = FALSE))
  }

  ## default converter is "ttm" if all exercises are Rnw, otherwise "pandoc"
  if(is.null(converter)) {
    converter <- if(any(tolower(tools::file_ext(unlist(file))) == "rmd")) "pandoc" else "ttm"
  }

  ## set up .html transformer
  args <- list(...)
  if(is.null(args$base64)) {
    if(canvas) args$base64 <- FALSE
  }
  if(canvas) {
    quiztype <- args$quiztype
    if(is.null(quiztype)) quiztype <- "assignment"
    quiztype <- match.arg(quiztype, c("assignment", "practice_quiz", "graded_survey", "survey"))
    args$quiztype <- NULL
  }
  args$converter <- converter
  htmltransform <- do.call("make_exercise_transform_html", args)

  ## create a name
  if(is.null(name)) {
    name <- file_path_sans_ext(basename(template))
    xmlname <- "qti"
  } else {
    name <- gsub("\\s", "_", name)
    if(is_number1(name))
      name <- paste0("_", name)
    xmlname <- name
  }
  if(isTRUE(rds)) rds <- name

  ## generate the exam
  is.xexam <- FALSE
  if(is.list(file)) {
    if(any(grepl("exam1", names(file))))
      is.xexam <- TRUE
  }
  if(!is.xexam) {
    exm <- xexams(file, n = n, nsamp = nsamp,
      driver = list(
        sweave = list(quiet = quiet, pdf = FALSE, png = !svg, svg = svg,
          resolution = resolution, width = width, height = height,
          encoding = encoding, envir = envir, engine = engine),
        read = NULL, transform = htmltransform, write = NULL),
      dir = dir, edir = edir, tdir = tdir, sdir = sdir, verbose = verbose, rds = rds, seed = seed, points = points)
  } else {
    exm <- file
    rm(file)
  }

  ## start .xml assessement creation
  itembody <- list(num = num, mchoice = mchoice, schoice = schoice, cloze = cloze, string = string)

  for(i in c("num", "mchoice", "schoice", "cloze", "string")) {
    if(is.null(itembody[[i]])) itembody[[i]] <- list()
    if(is.list(itembody[[i]])) {
      if(is.null(itembody[[i]]$eval)) itembody[[i]]$eval <- eval
      itembody[[i]]$flavor <- flavor
      itembody[[i]] <- do.call("make_itembody_qti12", itembody[[i]])
    }
    if(!is.function(itembody[[i]])) stop(sprintf("wrong specification of %s", sQuote(i)))
  }

  ## create a temporary directory
  dir <- path.expand(dir)
  if(is.null(tdir)) {
    dir.create(tdir <- tempfile())
    on.exit(unlink(tdir))
  } else {
    tdir <- path.expand(tdir)
  }
  if(!file.exists(tdir)) dir.create(tdir)

  ## the package directory
  pkg_dir <- find.package("exams")

  ## get the .xml template
  template <- path.expand(template)
  template <- ifelse(
    tolower(substr(template, nchar(template) - 3L, nchar(template))) != ".xml",
    paste(template, ".xml", sep = ""), template)
  template <- ifelse(file.exists(template),
    template, file.path(pkg_dir, "xml", basename(template)))
  if(!all(file.exists(template))) {
    stop(paste("The following files cannot be found: ",
      paste(basename(template)[!file.exists(template)], collapse = ", "), ".", sep = ""))
  }
  xml <- readLines(template[1L])

  ## check template for section and item inclusion
  if(length(section_start <- grep("<section ident", xml, fixed = TRUE)) != 1L ||
    length(section_end <- grep("</section>", xml, fixed = TRUE)) != 1L) {
    stop(paste("The XML template", template,
      "must contain exactly one opening and closing <section> tag!"))
  }
  section <- xml[section_start:section_end]
  if(length(item_start <- grep("<item ident", section, fixed = TRUE)) != 1 ||
    length(item_end <- grep("</item>", section, fixed = TRUE)) != 1) {
    stop(paste("The XML template", template,
      "must contain exactly one opening and closing <item> tag!"))
  }
  xml <- c(xml[1:(section_start - 1)], "##TestSections##", xml[(section_end + 1):length(xml)])
  item <- section[item_start:item_end]
  section <- section[1:(item_start - 1)]

  ## obtain the number of exams and questions
  nx <- length(exm)
  nq <- if(!is.xexam) length(exm[[1L]]) else length(exm)

  ## Canvas.
  media_dir_name <- if(!canvas) "media" else "data"

  ## function for internal ids
  make_test_ids <- function(n, type = c("test", "section", "item"))
  {
    switch(type,
      "test" = paste(name, make_id(9), sep = "_"),
      paste(type, formatC(1:n, flag = "0", width = nchar(n)), sep = "_")
    )
  }

  ## generate the test id
  test_id <- make_test_ids(type = "test")

  ## create section ids
  sec_ids <- paste(test_id, make_test_ids(nq, type = "section"), sep = "_")

  ## convenience function for creating integer XML tags
  make_tag <- function(x, type, default = 1, ...) {
    if(is.null(x)) x <- Inf
    x <- round(as.numeric(x), ...)
    if(x < default) {
      warning(paste("invalid ", type, " specification, ", type, "=", default, " used", sep = ""))
      x <- default
    }
    if(is.finite(x)) sprintf("%s=\"%s\"", type, x) else ""
  }

  ## create section/item titles and section description
  if(is.null(stitle)) stitle <- ""
  stitle <- rep(stitle, length.out = nq)
  if(!is.null(ititle)) ititle <- rep(ititle, length.out = nq)
  if(is.null(adescription)) adescription <- ""
  if(is.null(sdescription)) sdescription <- ""
  sdescription <- rep(sdescription, length.out = nq)

  ## enable different maxattempts per sections
  maxattempts <- rep(maxattempts, length.out = nq)

  ## points setting
  points <- sapply(1:nq, function(j) c(exm[[1L]][[j]]$metainfo$points, NA_real_)[1L])
  points[is.na(points)] <- 1

  ## create the directory where the test is stored
  dir.create(test_dir <- file.path(file_path_as_absolute(tdir), name))

  ## cycle through all exams and questions
  items <- sec_xml <- NULL
  for(j in 1:nq) {
    ## first, create the section header
    sec_xml <- c(sec_xml, gsub("##SectionId##", sec_ids[j], section, fixed = TRUE))

    if(canvas) {
      pos <- grep("<selection_number>", sec_xml, fixed = TRUE)[j]
      sec_xml <- c(
        sec_xml[1:pos],
        '<selection_extension>',
        paste0('<points_per_item>', sum(points[j]), '</points_per_item>'),
        '</selection_extension>',
        sec_xml[(pos + 1L):length(sec_xml)]
      )
    }

    sec_xml <- gsub("##SectionTitle##", stitle[j], sec_xml, fixed = TRUE)
    sec_xml <- gsub("##SectionDescription##", sdescription[j], sec_xml, fixed = TRUE)

    if(is.xexam) nx <- length(exm[[j]])
    item_ids <- paste(sec_ids[j], make_test_ids(nx, type = "item"), sep = "_")

    for(i in 1:nx) {
      if(is.xexam) {
        if(i < 2) jk <- j
        j <- i
        i <- jk
      }

      type <- exm[[i]][[j]]$metainfo$type
      iname <- paste(item_ids[if(is.xexam) j else i], type, sep = "_")
      exm[[i]][[j]]$metainfo$id <- iname

      ibody <- gsub("##ItemBody##",
        paste(thebody <- itembody[[type]](exm[[i]][[j]]), collapse = "\n"),
        item, fixed = TRUE)

      enumerate <- attr(thebody, "enumerate")
      if(is.null(enumerate)) enumerate <- FALSE
      xsolution <- exm[[i]][[j]]$solution
      if(!is.null(exm[[i]][[j]]$solutionlist)) {
        if(!all(is.na(exm[[i]][[j]]$solutionlist))) {
          xsolution <- c(xsolution, if(length(xsolution)) "<br />" else NULL)
          xsolution <- c(xsolution, if(enumerate) '<ol type = "a">' else '<ul>')
          if(exm[[i]][[j]]$metainfo$type == "cloze") {
            g <- rep(seq_along(exm[[i]][[j]]$metainfo$solution), sapply(exm[[i]][[j]]$metainfo$solution, length))
            ql <- sapply(split(exm[[i]][[j]]$questionlist, g), paste, collapse = " / ")
            sl <- sapply(split(exm[[i]][[j]]$solutionlist, g), paste, collapse = " / ")
          } else {
            ql <- exm[[i]][[j]]$questionlist
            sl <- exm[[i]][[j]]$solutionlist
          }
          nsol <- length(ql)
          xsolution <- c(xsolution, paste(rep('<li>', nsol),
            ql, if(length(exm[[i]][[j]]$solutionlist)) "<br />" else NULL,
            sl, rep('</li>', nsol)),
            if(enumerate) '</ol>' else '</ul>')
        }
      }

      ibody <- gsub("##ItemSolution##", paste(xsolution, collapse = "\n"), ibody, fixed = TRUE)
      ibody <- gsub("##ItemId##", iname, ibody, fixed = TRUE)

      if(any(grepl("##QuestionType##", ibody, fixed = TRUE))) {
        type2 <- switch(type,
          "schoice" = "SINGLE CHOICE QUESTION",
          "mchoice" = "MULTIPLE CHOICE QUESTION",
          "num" = "NUMERIC QUESTION",
          "cloze" = "CLOZE QUESTION",
          "string" = "TEXT QUESTION" 
        )
        ibody <- gsub("##QuestionType##", type2, ibody, fixed = TRUE)
      }

      ibody <- gsub("##ItemTitle##",
        if(is.null(ititle) | flavor == "ilias") exm[[i]][[j]]$metainfo$name else ititle[j],
        ibody, fixed = TRUE)

      if(length(exm[[i]][[j]]$supplements)) {
        if(!file.exists(media_dir <- file.path(test_dir, media_dir_name))) dir.create(media_dir)
        if(!file.exists(file.path(media_dir, sup_dir <- sprintf("supplements_%s_%s", i, j)))) dir.create(ms_dir <- file.path(media_dir, sup_dir))
        for(si in seq_along(exm[[i]][[j]]$supplements)) {
          file.copy(exm[[i]][[j]]$supplements[si],
            file.path(ms_dir, f <- basename(exm[[i]][[j]]$supplements[si])))
          if(any(grepl(dirname(exm[[i]][[j]]$supplements[si]), ibody))) {
            ibody <- gsub(dirname(exm[[i]][[j]]$supplements[si]),
              file.path(media_dir_name, sup_dir), ibody, fixed = TRUE)
          } else {
            if(any(grepl(f, ibody))) {
              ibody <- gsub(paste(f, '"', sep = ''),
                paste(paste0(media_dir_name, '/'), sup_dir, '/', f, '"', sep = ''), ibody, fixed = TRUE)
            }
          }
        }
      }

      sec_xml <- c(sec_xml, ibody, "")
    }

    sec_xml <- c(sec_xml, "", "</section>")
    maxattempts_tag <- make_tag(nmax0 <- maxattempts[j], type = "maxattempts", default = 1)
    sec_xml <- gsub("##MaxAttempts##", maxattempts_tag, sec_xml, fixed = TRUE)
  }

  if(any(maxattempts != 1L) && solutionswitch) {
    warning("if solutionswitch is TRUE, maxattempts should typically be 1 so that the solution cannot be copied by participants")
  }

  if(!is.null(duration)) {
    dur0 <- duration
    dursecs <- round(duration * 60)
    dur <- dursecs %/% 86400 
    dursecs <- dursecs - dur * 86400
    duration <- paste("P0Y0M", dur, "DT", sep = "")
    dur <- dursecs %/% 3600 
    dursecs <- dursecs - dur * 3600
    duration <- paste(duration, dur, "H", sep = "")
    dur <- dursecs %/% 60 
    dursecs <- dursecs - dur * 60
    duration <- paste("<duration>", duration, dur, "M", dursecs, "S", "</duration>", sep = "")
  } else {
    dur0 <- duration <- ""
  }

  cutvalue <- make_tag(cutvalue, type = "cutvalue", default = 0, digits = 8)

  feedbackswitch <- FALSE 
  hintswitch <- FALSE
  xml <- gsub("##TestIdent##", test_id, xml, fixed = TRUE)
  xml <- gsub("##TestTitle##", name, xml, fixed = TRUE)
  xml <- gsub("##TestDuration##", if(canvas) dur0 else duration, xml, fixed = TRUE)
  xml <- gsub("##TestSections##", paste(sec_xml, collapse = "\n"), xml, fixed = TRUE)
  xml <- gsub("##CutValue##", cutvalue, xml, fixed = TRUE)
  xml <- gsub("##FeedbackSwitch##", if(feedbackswitch) "Yes" else "No", xml, fixed = TRUE)
  xml <- gsub("##HintSwitch##",     if(hintswitch)     "Yes" else "No", xml, fixed = TRUE)
  xml <- gsub("##SolutionSwitch##", if(solutionswitch) "Yes" else "No", xml, fixed = TRUE)
  xml <- gsub("##AssessmentDescription##", adescription, xml, fixed = TRUE)

  if(canvas) {
    pos <- grep('<qtimetadata>', xml, fixed = TRUE)[1L]
    xml <- c(
      xml[1:pos],
      if(dur0 != "") {
        c('<qtimetadatafield>',
          '<fieldlabel>qmd_timelimit</fieldlabel>',
          paste0('<fieldentry>', dur0, '</fieldentry>'),
          '</qtimetadatafield>')
      } else NULL,
      if(is.finite(nmax0)) {
        c('<qtimetadatafield>',
          '<fieldlabel>cc_maxattempts</fieldlabel>',
          paste0('<fieldentry>', nmax0, '</fieldentry>'),
        '</qtimetadatafield>')
      } else NULL,
      xml[(pos + 1L):length(xml)]
    )
  }

  if(!identical(xmlcollapse, FALSE)) {
    xmlcollapse <- if(identical(xmlcollapse, TRUE)) " " else as.character(xmlcollapse)

    pre1 <- grep("<pre>", xml, fixed = TRUE)
    pre2 <- grep("</pre>", xml, fixed = TRUE)
    if(length(pre1) != length(pre2)) warning("cannot properly fix <pre> tags")
    if(length(pre1) > 0L) {
      for(i in length(pre1):1L) {
        p1 <- pre1[i]
        p2 <- pre2[i]
        if(p2 > p1) {
          xml[p1] <- paste(xml[p1:p2], collapse = "\n")
          xml <- xml[-((p1 + 1L):p2)]
        }
      }
    }

    xml <- paste(xml, collapse = " ")
  }

  if(canvas) {
    data_supps <- dir(file.path(test_dir, media_dir_name), recursive = TRUE, full.names = FALSE, include.dirs = FALSE)

    for(j in data_supps) {
      xml <- gsub(paste0('alt="', media_dir_name, '/', j, '"'),
        paste0('alt="', basename(j), '"'), xml, fixed = TRUE)
    }

    quiz_id <- make_test_ids(type = "test")

    dir.create(file.path(test_dir, quiz_id))

    writeLines(xml, file.path(test_dir, quiz_id, paste(quiz_id, "xml", sep = ".")))

    template_canvas <- file.path(pkg_dir, "xml", "canvas_meta.xml")
    xml_meta <- readLines(template_canvas)

    xml_meta <- gsub("##QuizIdent##", quiz_id, xml_meta, fixed = TRUE)
    xml_meta <- gsub("##TestIdent##", test_id, xml_meta, fixed = TRUE)
    xml_meta <- gsub("##AssignmentIdent##", paste0('AID_', test_id), xml_meta, fixed = TRUE)
    xml_meta <- gsub("##GroupIdent##", paste0('GID_', test_id), xml_meta, fixed = TRUE)
    xml_meta <- gsub("##TestTitle##", name, xml_meta, fixed = TRUE)
    xml_meta <- gsub("##TestDuration##", dur0, xml_meta, fixed = TRUE)
    xml_meta <- gsub("##MaxAttempts##", nmax0, xml_meta, fixed = TRUE)
    xml_meta <- gsub("##AssessmentDescription##", adescription, xml_meta, fixed = TRUE)
    xml_meta <- gsub("##Points##", sum(unlist(points)), xml_meta, fixed = TRUE)

    xml_meta <- gsub("##QuizType##", quiztype, xml_meta, fixed = TRUE)
    if(quiztype != "assignment") {
      aid <- c(grep("<assignment identifier=", xml_meta, fixed = TRUE), grep("</assignment>", xml_meta, fixed = TRUE))
      if(length(aid) == 2L && (aid[2L] > aid[1L])) xml_meta <- xml_meta[-(aid[1L]:aid[2L])]
    }

    writeLines(xml_meta, file.path(test_dir, quiz_id, "assessment_meta.xml"))

    template_canvas <- file.path(pkg_dir, "xml", "canvas_manifest.xml")
    manifest <- readLines(template_canvas)

    manifest <- gsub("##ManifestIdent##", paste0('MID_', test_id), manifest, fixed = TRUE)
    manifest <- gsub("##ManifestTitle##", name, manifest, fixed = TRUE)
    manifest <- gsub("##Date##", Sys.Date(), manifest, fixed = TRUE)

    resources <- c('<resources>',
      paste0('    <resource identifier="', quiz_id, '" type="imsqti_xmlv1p2">'),
      paste0('      <file href="', quiz_id, '/', paste(quiz_id, "xml", sep = "."), '"/>'),
      paste0('      <dependency identifierref="', paste0('MID_REF_', test_id), '"/>'),
      '    </resource>',
      paste0('    <resource identifier="', paste0('MID_REF_', test_id),
        '" type="associatedcontent/imscc_xmlv1p1/learning-application-resource" href="',
        quiz_id, '/assessment_meta.xml">'),
      paste0('      <file href="', quiz_id, '/assessment_meta.xml"/>'),
      '    </resource>'
    )

    sid <- 1234
    for(j in data_supps) {
      resources <- c(resources,
        paste0('    <resource href="', media_dir_name, '/', j, '" identifier="', sid, '" type="webcontent">'),
        paste0('      <file href="', media_dir_name, '/', j,'"/>'),
        '    </resource>'
      )
      sid <- sid + 1L
    }

    resources <- paste0(c(resources, '  </resources>'), collapse = '\n')

    manifest <- gsub("##Resources##", resources, manifest, fixed = TRUE)

    writeLines(manifest, file.path(test_dir, "imsmanifest.xml"))
  } else {
    writeLines(xml, file.path(test_dir, if(zip) "qti.xml" else paste(xmlname, "xml", sep = ".")))
  }

  if(zip) {
    owd <- getwd()
    setwd(test_dir)
    zip(zipfile = zipname <- paste(name, "zip", sep = "."), files = list.files(test_dir))
    setwd(owd)
  } else zipname <- list.files(test_dir)

  file.copy(file.path(test_dir, zipname), dir, recursive = TRUE)

  attr(exm, "test_id") <- test_id

  invisible(exm)
}


.empty_text <- function(x) {
  is.null(x) || anyNA(x) || all(grepl("^[[:space:]]*$", x))
}

make_itembody_qti12 <- function(rtiming = FALSE, shuffle = FALSE, rshuffle = shuffle,
  minnumber = NULL, maxnumber = NULL, defaultval = NULL, minvalue = NULL,
  maxvalue = NULL, cutvalue = NULL, enumerate = FALSE, digits = NULL, tolerance = is.null(digits),
  maxchars = 12, eval = list(partial = TRUE, rule = "false2", negative = FALSE), fix_num = TRUE,
  flavor = "plain")
{
  function(x) {
    flavor <- match.arg(flavor, c("plain", "openolat", "canvas", "ilias"))
    canvas <- flavor == "canvas"
    if(canvas)
      fix_num <- FALSE

    points <- if(is.null(x$metainfo$points)) 1 else x$metainfo$points

    solution <- if(!is.list(x$metainfo$solution)) {
      list(x$metainfo$solution)
    } else x$metainfo$solution
    n <- length(solution)

    questionlist <- if(!is.list(x$questionlist)) {
      if(x$metainfo$type == "cloze") {
        g <- rep(seq_along(x$metainfo$solution), sapply(x$metainfo$solution, length))
        split(x$questionlist, g)
      } else list(x$questionlist)
    } else {
      x$questionlist
    }
    
    if(length(questionlist) < 1) {
      questionlist <- NULL
    } else if(flavor == "ilias") {
      questionlist <- lapply(questionlist, function(q) {
        q <- gsub("<code>", '<code style="display:inline-block; vertical-align:middle; font-family:monospace; background-color:#f8f9fa; padding:2px 6px; border:1px solid #ddd; border-radius:3px;">', q, fixed = TRUE)
        ifelse(grepl("<span", q, fixed = TRUE), q, paste0("<span>", q, "</span>"))
      })
    }

    tol <- if(!is.list(x$metainfo$tolerance)) {
      if(x$metainfo$type == "cloze") as.list(x$metainfo$tolerance) else list(x$metainfo$tolerance)
    } else x$metainfo$tolerance
    tol <- rep(tol, length.out = n)

    if((length(points) == 1) & (x$metainfo$type == "cloze"))
      points <- points / n

    q_points <- rep(points, length.out = n)
    if(x$metainfo$type == "cloze")
      points <- sum(q_points)

    type <- x$metainfo$type
    type <- if(type == "cloze") x$metainfo$clozetype else rep(type, length.out = n)

    if(is.null(eval) || length(eval) < 1L) eval <- exams_eval()
    if(!is.list(eval)) stop("'eval' needs to specify a list of partial/negative/rule")
    eval <- eval[match(c("partial", "negative", "rule"), names(eval), nomatch = 0)]
    if(x$metainfo$type %in% c("num", "string", "schoice")) eval$partial <- FALSE
    eval <- do.call("exams_eval", eval)

    maxchars <- if(is.null(x$metainfo$maxchars)) {
        if(length(maxchars) < 2) {
           c(maxchars, NA, NA)
        } else maxchars[1:3]
    } else x$metainfo$maxchars
    if(!is.list(maxchars))
      maxchars <- list(maxchars)
    maxchars <- rep(maxchars, length.out = n)
    for(j in seq_along(maxchars)) {
      if(length(maxchars[[j]]) < 2)
        maxchars[[j]] <- c(maxchars[[j]], NA, NA)
    }

    xml <- c(
      '<presentation>',
      '<flow>',
      if(!is.null(x$question)) {
        c(
          '<material>',
          '<matbreak/>',
          '<mattext texttype="text/html" charset="utf-8"><![CDATA[',
          x$question,
          ']]></mattext>',
          '<matbreak/>',
          '</material>'
        )
      } else NULL
    )

    letters2 <- c(letters,
      paste0(sort(rep(letters, length(letters))),
      rep(letters, length(letters))))

    multiple_dropdowns <- FALSE

    ids <- el <- pv <- list()
    for(i in 1:n) {
      iid <- x$metainfo$id

      ids[[i]] <- list("response" = paste(iid, "RESPONSE", make_id(7), sep = "_"),
        "questions" = paste(iid, make_id(10, length(solution[[i]])), sep = "_"))

      ## THE 100% CORRECT FIX: The Boolean Trap Bypass
      ## Only run multiple-choice math on actual choice gaps.
      ## For strings and numbers, pass a dummy TRUE to safely extract the base points.
      if(type[i] %in% c("schoice", "mchoice")) {
        pv[[i]] <- eval$pointvec(solution[[i]])
      } else {
        pv[[i]] <- eval$pointvec(TRUE)
      }

      pv[[i]]["pos"] <- pv[[i]]["pos"] * q_points[i]
      if(length(grep("choice", type[i])))
        pv[[i]]["neg"] <- pv[[i]]["neg"] * q_points[i]

      ans <- FALSE 

      if(length(grep("choice", type[i]))) {

        if(canvas & (type[i] == "schoice")) {
          if(any(grepl(asub <- paste0("##ANSWER", i, "##"), xml))) {
            xml <- gsub(asub, paste0("[", ids[[i]]$response, "]"), xml, fixed = TRUE)
            multiple_dropdowns <- TRUE
          }
        }

        txml <- c(
          paste('<response_lid ident="', ids[[i]]$response, '" rcardinality="',
            if(type[i] == "mchoice") "Multiple" else "Single", '" rtiming=',
            if(rtiming) '"Yes"' else '"No"', '>', sep = ''),
          paste('<render_choice shuffle="', if(shuffle) 'Yes' else 'No', '">', sep = '')
        )
        
        for(j in seq_along(solution[[i]])) {
          if(multiple_dropdowns) questionlist[[i]][j] <- pandoc(questionlist[[i]][j], from = "html", to = "plain")
        
          txml <- c(txml,
            '<flow_label class="List">',
            paste('<response_label ident="', ids[[i]]$questions[j], '" rshuffle="',
              if(rshuffle) 'Yes' else 'No', '">', sep = ''),
            '<material>',
            '<mattext texttype="text/html" charset="utf-8"><![CDATA[',
             paste(if(enumerate & n > 1) {
               paste(letters2[if(x$metainfo$type == "cloze") i else j], ".",
                 if(x$metainfo$type == "cloze" && length(solution[[i]]) > 1) paste(j, ".", sep = "") else NULL,
                 sep = "")
             } else NULL, questionlist[[i]][j]),
            ']]></mattext>',
            '</material>',
            '</response_label>',
            '</flow_label>'
          )
        }

        txml <- c(txml,
          '</render_choice>',
          '</response_lid>'
        )
      }
      if(type[i] == "string" || type[i] == "num") {
        for(j in seq_along(solution[[i]])) {
          soltext <- if(type[i] == "num") {
             if(!is.null(digits)) format(round(solution[[i]][j], digits), nsmall = digits) else solution[[i]][j]
          } else {
            if(!is.character(solution[[i]][j])) format(solution[[i]][j]) else solution[[i]][j]
          }
          qlc <- .empty_text(questionlist[[i]][j])
          txml <- c(
            if(!qlc) {
              c('<material>',
                paste('<mattext><![CDATA[', paste(if(enumerate & n > 1) {
                  paste(letters2[i], ".", sep = '')
                } else NULL, questionlist[[i]][j]), ']]></mattext>', sep = ""),
                '</material>',
                '<material>', '<matbreak/>', '</material>'
              )
            } else NULL,
            paste(
          if(type[i] == "string") {
            '<response_str ident="'
          } else if(!tolerance | fix_num) {
            '<response_str ident="'
          } else {
            '<response_num ident="'
              },
          ids[[i]]$response,
          if(flavor == "ilias" && !is.na(maxchars[[i]][3])) {
            '" rcardinality="Ordered"' 
           } else {
            '" rcardinality="Single"'
           },
          if(!(!tolerance | fix_num)) ' numtype="Decimal"' else NULL,
          '>', sep = ''),
            paste('<render_fib', if(!(!tolerance | fix_num)) ' fibtype="Decimal"' else NULL,
              if(!is.na(maxchars[[i]][1])) {
                paste(' maxchars="', max(c(nchar(soltext), maxchars[[i]][1])), '"', sep = '')
              } else NULL,
              if(!is.na(maxchars[[i]][2])) {
                paste(' rows="', maxchars[[i]][2], '"', sep = '')
              } else NULL,
              if(!is.na(maxchars[[i]][3])) {
                paste(' columns="', maxchars[[i]][3], '"', sep = '')
              } else NULL,
              if(flavor == "ilias" && !is.na(maxchars[[i]][3])) {
                ' fibtype="String" prompt="Box"'
              } else NULL,  
          '>', sep = ''),
            '<flow_label class="Block">',
            paste('<response_label ident="', ids[[i]]$response, '" rshuffle="No"/>', sep = ''),
            '</flow_label>',
            '</render_fib>',
            if(type[i] == "string") '</response_str>' else {
              if(!tolerance | fix_num) '</response_str>' else '</response_num>'
            },
            '<material>', '<matbreak/>', '</material>'
          )
        }
      }

      if(ans) {
        txml <- paste(txml, collapse = '\n')
        xml <- gsub(paste0("##ANSWER", i, "##"), txml, xml, fixed = TRUE)
      } else {
        xml <- c(xml, txml)
      }
    }

    if(canvas) {
      canvas_type <- if(multiple_dropdowns) {
        "multiple_dropdowns_question"
      } else {
        if(length(type) > 1L) stop("only cloze questions with schoice elements are supported for Canvas")
        switch(type,
          "num" = "numerical_question",
          "schoice" = "multiple_choice_question",
          "mchoice" = "multiple_answers_question",
          "string" = "short_answer_question"
        )
      }
      if(identical(type, "string") && !is.null(stringtype <- x$metainfo$stringtype) && !identical(x$metainfo$stringtype, "string")) {
        if(length(stringtype) > 1L) {
          stringtype <- stringtype[1L]
          warning(sprintf("for Canvas only a single 'stringtype' is supported, using the first: %s", stringtype))
        }
        if(stringtype == "essay") canvas_type <- "essay_question"
        if(stringtype == "file") canvas_type <- "file_upload_question"
      }
      xml <- c(
        '<itemmetadata>',
        '<qtimetadata>',
        '<qtimetadatafield>',
        '<fieldlabel>question_type</fieldlabel>',
        paste0('<fieldentry>', canvas_type, '</fieldentry>'),
        '</qtimetadatafield>',
        if(multiple_dropdowns) {
          c('<qtimetadatafield>',
          '<fieldlabel>original_answer_ids</fieldlabel>',
          paste0('<fieldentry>', paste0(ids[[i]]$questions, collapse = ','), '</fieldentry>'),
          '</qtimetadatafield>')
        } else NULL,
        '</qtimetadata>',
        '</itemmetadata>',
        xml
      )
    }

    xml <- c(xml, '</flow>', '</presentation>')

    if(is.null(minvalue)) {  
      if(eval$negative) {
        minvalue <- sum(sapply(pv, function(x) { x["neg"] }))
      } else minvalue <- 0
    }

    xml <- c(xml,
      '<resprocessing>',
      '<outcomes>',
      paste('<decvar varname="SCORE" vartype="Decimal" defaultval="',
        if(is.null(defaultval)) 0 else defaultval, '" minvalue="',
        if(is.null(minvalue) | is.na(minvalue)) 0 else minvalue, '" maxvalue="',
        if(is.null(maxvalue)) points else maxvalue, '" cutvalue="',
        if(is.null(cutvalue)) points else cutvalue, '"/>', sep = ''),
      '</outcomes>')

    correct_answers <- wrong_answers <- correct_num <- wrong_num <- vector(mode = "list", length = n)
    for(i in 1:n) {
      if(length(grep("choice", type[i]))) {

        for(j in seq_along(solution[[i]])) {
          if(solution[[i]][j]) {
            correct_answers[[i]] <- c(correct_answers[[i]],
              paste('<varequal respident="', ids[[i]]$response,
                '" case="Yes">', ids[[i]]$questions[j], '</varequal>', sep = '')
            )
          } else {
            wrong_answers[[i]] <- c(wrong_answers[[i]],
              paste('<varequal respident="', ids[[i]]$response,
                '" case="Yes">', ids[[i]]$questions[j], '</varequal>', sep = '')
            )
          }
        }
      }
      if(type[i] == "string" || type[i] == "num") {
        for(j in seq_along(solution[[i]])) {
          if(type[i] == "string") {
            soltext <- if(!is.character(solution[[i]][j])) {
              format(round(solution[[i]][j], digits), nsmall = digits)
            } else solution[[i]][j]
            correct_answers[[i]] <- c(correct_answers[[i]], paste('<varequal respident="', ids[[i]]$response,
              '" case="No"><![CDATA[', soltext, ']]></varequal>', sep = "")
            )
          } else {
            correct_answers[[i]] <- c(correct_answers[[i]],
              if(!tolerance) {
                paste('<varequal respident="', ids[[i]]$response,
                  '" case="No"><![CDATA[', if(!is.null(digits)) {
                    format(round(solution[[i]][j], digits), nsmall = digits)
                  } else solution[[i]][j],
                  ']]></varequal>', sep = "")
              } else {
                if(fix_num) {
                  correct_num[[i]] <- c(correct_num[[i]],
                    paste('<varequal respident="', ids[[i]]$response,
                      '" case="No"><![CDATA[', if(!is.null(digits)) {
                      format(round(solution[[i]][j], digits), nsmall = digits)
                      } else solution[[i]][j],
                      ']]></varequal>', sep = "")
                  )
                } 
                wrong_num[[i]] <- paste(
                  if(canvas) {
                    paste(c('\n<or>', paste('<varequal respident="', ids[[i]]$response,
                      '" case="No"><![CDATA[', if(!is.null(digits)) {
                      format(round(solution[[i]][j], digits), nsmall = digits)
                      } else solution[[i]][j],
                      ']]></varequal>\n', sep = "")), collapse = '\n', sep = '')
                  } else NULL,
                  '<and>\n',
                  paste('<vargte respident="', ids[[i]]$response, '">',
                    solution[[i]][j] - max(tol[[i]]),
                    '</vargte>\n', sep = ""),
                  paste('<varlte respident="', ids[[i]]$response, '">',
                    solution[[i]][j] + max(tol[[i]]),
                    '</varlte>\n', sep = ""),
                  '</and>',
                  if(canvas) '\n</or>' else NULL,
                  collapse = '\n', sep = ''
                )
              }
            )
          }
        }
      }
      if(!is.null(correct_answers[[i]])) {
        attr(correct_answers[[i]], "points") <- pv[[i]]
        attr(correct_answers[[i]], "type") <- type[i]
      }
      if(!is.null(wrong_answers[[i]]))
        attr(wrong_answers[[i]], "points") <- pv[[i]]
    }

    correct_answers <- delete.NULLs(correct_answers)
    wrong_answers <- delete.NULLs(wrong_answers)
    correct_num <- unlist(delete.NULLs(correct_num))
    wrong_num <- delete.NULLs(wrong_num)
    if(length(wrong_num)) {
      wrong_num <- sapply(wrong_num, function(x) {
        paste('<not>', x, '</not>', collapse = '\n')
      })
      wrong_num <- unlist(wrong_num)
    }

    is_ilias <- identical(flavor, "ilias")
    is_ilias_choice <- is_ilias && (x$metainfo$type %in% c("mchoice", "schoice"))

    if(is_ilias_choice) {
      if (x$metainfo$type == "schoice") {
        xml <- c(xml,
          '<respcondition title="Mastery" continue="Yes">',
          '<conditionvar>',
          unlist(correct_answers),
          '</conditionvar>',
          paste('<setvar varname="SCORE" action="Add">', points, '</setvar>', sep = ''),
          '<displayfeedback feedbacktype="Response" linkrefid="Mastery"/>',
          '</respcondition>'
        )
      } else if (x$metainfo$type == "mchoice") {
        if(length(correct_answers)) {
          for(i in seq_along(correct_answers)) {
            for(j in correct_answers[[i]]) {
              pts <- if(eval$partial) attr(correct_answers[[i]], "points")["pos"] else points
              xml <- c(xml,
                '<respcondition continue="Yes">',
                '<conditionvar>', j, '</conditionvar>',
                paste0('<setvar varname="SCORE" action="Add">', pts, '</setvar>'),
                '</respcondition>',
                '<respcondition continue="Yes">',
                '<conditionvar><not>', j, '</not></conditionvar>',
                '<setvar varname="SCORE" action="Add">0</setvar>',
                '</respcondition>'
              )
            }
          }
        }
        if(length(wrong_answers)) {
          for(i in seq_along(wrong_answers)) {
            for(j in wrong_answers[[i]]) {
              pts <- if(eval$partial) attr(wrong_answers[[i]], "points")["neg"] else 0
              xml <- c(xml,
                '<respcondition continue="Yes">',
                '<conditionvar>', j, '</conditionvar>',
                paste0('<setvar varname="SCORE" action="Add">', pts, '</setvar>'),
                '</respcondition>',
                '<respcondition continue="Yes">',
                '<conditionvar><not>', j, '</not></conditionvar>',
                '<setvar varname="SCORE" action="Add">0</setvar>',
                '</respcondition>'
              )
            }
          }
        }
      }
    } else {
      if((eval$partial | x$metainfo$type == "cloze") & !multiple_dropdowns) {
        if(length(correct_answers)) {
          for(i in seq_along(correct_answers)) {
            for(j in correct_answers[[i]]) {
              xml <- c(xml,
                '<respcondition continue="Yes" title="Mastery">',
                '<conditionvar>',
                j,
                '</conditionvar>',
                paste('<setvar varname="SCORE" action="Add">',
                  attr(correct_answers[[i]], "points")["pos"], '</setvar>', sep = ''),
                '</respcondition>'
              )
            }
          }
        }
        if(length(wrong_answers)) {
          for(i in seq_along(wrong_answers)) {
            for(j in wrong_answers[[i]]) {
              xml <- c(xml,
                '<respcondition continue="Yes" title="Fail">',
                '<conditionvar>',
                j,
                '</conditionvar>',
                paste('<setvar varname="SCORE" action="Add">',
                  attr(wrong_answers[[i]], "points")["neg"], '</setvar>', sep = ''),
                '<displayfeedback feedbacktype="Solution" linkrefid="Solution"/>',
                '</respcondition>'
              )
            }
          }
        }
      }

      if(eval$partial & x$metainfo$type == "cloze") {
        if(length(correct_answers)) {
          for(i in seq_along(correct_answers)) {
            ctype <- attr(correct_answers[[i]], "type")
            if(ctype == "string" || ctype == "num" || ctype == "schoice") {
              xml <- c(xml,
                '<respcondition title="Fail" continue="Yes">',
                '<conditionvar>',
                '<not>',
                correct_answers[[i]],
                '</not>',
                '</conditionvar>',
                paste('<setvar varname="SCORE" action="Add">',
                  attr(correct_answers[[i]], "points")["neg"], '</setvar>', sep = ''),
                '<displayfeedback feedbacktype="Solution" linkrefid="Solution"/>',
                '</respcondition>'
              )
            }
          }
        }
      }

      if(!multiple_dropdowns) {
        xml <- c(xml,
          paste('<respcondition title="Mastery"', if(canvas) 'continue="No">' else ' continue="Yes">'),
          '<conditionvar>',
          if(!is.null(correct_answers) & (length(correct_answers) > 1 | grepl("choice", x$metainfo$type))) '<and>' else NULL
        )

        xml <- c(xml,
          unlist(correct_answers),
          if(!is.null(correct_answers) & (length(correct_answers) > 1 | grepl("choice", x$metainfo$type))) {
              if(canvas) NULL else '</and>'
            } else { NULL },
          if(!is.null(wrong_answers)) {
            if(canvas) {
              c(paste('<not>', unlist(wrong_answers), '</not>'), '</and>')
            } else {
              c('<not>', '<or>', unlist(wrong_answers), '</or>', '</not>')
            }
          } else {
            NULL
          },
          '</conditionvar>',
          if(!eval$partial) {
            paste('<setvar varname="SCORE" action="Set">', points, '</setvar>', sep = '')
          } else NULL,
          paste('<displayfeedback feedbacktype="Response"', if(canvas) 'linkrefid="correct_fb"/>' else 'linkrefid="Mastery"/>'),
          '</respcondition>'
        )
      } else {
        for(i in seq_along(correct_answers)) {
          xml <- c(xml,
            '<respcondition>',
            '<conditionvar>',
            correct_answers[[i]],
            '</conditionvar>',
            paste0('<setvar varname="SCORE" action="Add">', if(eval$partial) attr(correct_answers[[i]], "points")["pos"] else points, '</setvar>'),
            '</respcondition>'
          )
        }
      }

      if(length(correct_num)) {
        for(j in correct_num) {
          xml <- c(xml,
            '<respcondition continue="Yes" title="Mastery">',
            '<conditionvar>', j, '</conditionvar>',
            if(fix_num) {
              c(paste('<setvar varname="SCORE" action="Add">', 0.001, '</setvar>', sep = ''),
                paste('<setvar varname="SCORE" action="Add">', -0.001, '</setvar>', sep = ''))
            } else NULL,
            '</respcondition>'
          )
        }
      }

      if(length(correct_answers)) {
        for(j in seq_along(correct_answers)) {
          if(attr(correct_answers[[j]], "type") != "num") {
            if(canvas & grepl("choice", attr(correct_answers[[j]], "type"))) {
              if((length(correct_answers) > 1L) & !multiple_dropdowns) {
                xml <- c(xml, '<respcondition continue="Yes" title="Mastery">', '<conditionvar>', correct_answers[[j]], '</conditionvar>', '</respcondition>')
              }
            } else {
              xml <- c(xml, '<respcondition continue="Yes" title="Mastery">', '<conditionvar>', correct_answers[[j]], '</conditionvar>', '</respcondition>')
            }
          }
        }
      }

      correct_answers <- unlist(correct_answers)
      wrong_answers <- c(unlist(wrong_answers), unlist(wrong_num))

      if(!eval$partial & x$metainfo$type == "cloze") {
        if(length(correct_answers) & !multiple_dropdowns) {
          for(i in seq_along(correct_answers)) {
              xml <- c(xml, '<respcondition title="Fail" continue="Yes">', '<conditionvar>', '<not>', correct_answers[[i]], '</not>', '</conditionvar>', paste('<setvar varname="SCORE" action="Add">', -1 * n * points, '</setvar>', sep = ''), '<displayfeedback feedbacktype="Solution" linkrefid="Solution"/>', '</respcondition>')
          }
        }
        if(length(wrong_answers) & !multiple_dropdowns) {
          for(i in seq_along(wrong_answers)) {
            for(j in wrong_answers[[i]]) {
              xml <- c(xml, '<respcondition continue="Yes" title="Fail">', '<conditionvar>', j, '</conditionvar>', paste('<setvar varname="SCORE" action="Add">', -1 * n * points, '</setvar>', sep = ''), '<displayfeedback feedbacktype="Solution" linkrefid="Solution"/>', '</respcondition>')
            }
          }
        }
      }

      if(!canvas) {
        xml <- c(xml,
          '<respcondition title="Fail" continue="Yes">',
          '<conditionvar>',
          if(!is.null(wrong_answers)) NULL else '<not>',
          if(is.null(wrong_answers)) {
            c(if(length(correct_answers) > 1) '<and>' else NULL, correct_answers, if(length(correct_answers) > 1) '</and>' else NULL)
          } else {
            c('<or>', wrong_answers, '</or>')
          },
          if(!is.null(wrong_answers)) NULL else '</not>',
          '</conditionvar>',
          if(!eval$partial & !is.na(minvalue)) { paste('<setvar varname="SCORE" action="Set">', minvalue, '</setvar>', sep = '') } else NULL,
          '<displayfeedback feedbacktype="Solution" linkrefid="Solution"/>',
          '</respcondition>',
          '<respcondition title="Fail" continue="Yes">', '<conditionvar>', '<other/>', '</conditionvar>', paste('<setvar varname="SCORE" action="Set">', 0, '</setvar>', sep = ''), '<displayfeedback feedbacktype="Solution" linkrefid="Solution"/>', '</respcondition>'
        )
      } else {
        xml <- c(xml, '<respcondition continue="Yes">', '<conditionvar>', '<other/>', '</conditionvar>', '<displayfeedback feedbacktype="Response" linkrefid="general_incorrect_fb"/>', '</respcondition>')
      }
    }

    xml <- c(xml, '</resprocessing>')
    attr(xml, "enumerate") <- enumerate
    xml
  }
}

make_id <- function(size, n = 1L) {
  if(is.null(n)) n <- 1L
  rval <- matrix(sample(0:9, size * n, replace = TRUE), ncol = n, nrow = size)
  rval[1L, ] <- pmax(1L, rval[1L, ])
  colSums(rval * 10^((size - 1L):0L))
}

delete.NULLs <- function(x.list) {
  rval <- x.list[unlist(lapply(x.list, length) != 0)]
  rval <- if(length(rval)) rval else NULL
  rval
}
