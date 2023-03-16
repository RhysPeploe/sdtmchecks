#' @title Check for duplicate SA entries
#'
#' @description Identifies duplicated AE entries based on USUBJID, AETERM,
#'   AEDECOD, AESTDTC, AEENDTC, AEMODIFY (if present), AELAT (if present) and AETOXGR or AESEV
#'
#' @param AE AE SDTM dataset with variables USUBJID, AETERM, AEDECOD,
#'   AESTDTC, AEENDTC, and AETOXGR or AESEV
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @importFrom dplyr %>% group_by_all filter select n
#' @importFrom tidyselect any_of
#'
#' @author Edgar Manukyan
#'
#' @examples
#'
#' AE <- data.frame(USUBJID = c(1), AESTDTC = c("2020-01-01","2020-01-01","2020-02-01","2020-03-01"),
#'                  AEENDTC = rep("2020-02-01",4), AEDECOD = letters[c(1,1:3)],
#'                  AETERM = letters[c(1,1:3)], AETOXGR = c(1,1:3),
#'                  AESPID="FORMNAME-R:5/L:5XXXX",
#'                  stringsAsFactors=FALSE)
#'
#' check_sa_dup(SA)
#'
#'
#'

check_sa_dup <- function(SA){

    # Checks whether required variables are in dataset
    if (SA %lacks_any% c("USUBJID",  "SATERM")) {  ##"SADECOD", "AESTDTC", "AEENDTC",

        fail(lacks_msg(SA, c("USUBJID", "SATERM"))) ## "AEDECOD", "AESTDTC", "AEENDTC"

    } else if (SA %has_all% c("SASEV")) { ##"AETOXGR", 

        fail("SA has both variables: AESEV.") ##AETOXGR and 

    } else if (SA %lacks_all% c( "SASEV")) { ##"AETOXGR",

        fail("SA is missing thE SASEV variable.") ## 

    } else {

        # Use either AETOXGR or AESEV, depending on which is in the AE dataset
        ##toxgr_var <- "AESEV" ##if(AE %has_all% "AETOXGR") "AETOXGR" else
        ##lat_var <- if (AE %has_all% "AELAT") "AELAT" else NULL

        if (SA %lacks_any% c("AEMODIFY")){
            # When AEMODIFY not in AE
            # Subsets to duplicated entries only
            df <- SA %>%
                select(USUBJID, SATERM, SASEV) %>%
                group_by_all() %>%
                filter(n()>1)
        }else {
            # When AEMODIFY in AE
            # Subsets to duplicated entries only
            df <- SA %>%
                select(USUBJID, SATERM, SASEV, SAMODIFY, any_of(c(toxgr_var,lat_var))) %>%
                group_by_all() %>%
                filter(n()>1)
        }

        # Outputs a resulting message depending on whether there are duplicates
        if (nrow(df) != 0) {

            fail("SA has duplicated entries. ", df)

        } else {

            pass()

        }
    }
}

