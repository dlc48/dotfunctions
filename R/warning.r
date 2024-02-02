#' .w
#'
#' @param x input
#' @export
.w=function(x=NULL){
    msg = c(
    # pendu 17
    paste(
    "\t_________\n",
    "\t|     |\n",
    "\t|     o\n",
    "\t|    /|\\ \n",
    "\t|    / \\ \n",
    "\t|\n",
    "\t|\n",
    "\t-\n\n",sep=""),
    # sad 62
    .p(
    "\t   d888888o.           .8.          8 888888888o.     \n",
    "\t .`8888:' `88.        .888.         8 8888    `^888.  \n",
    "\t 8.`8888.   Y8       :88888.        8 8888        `88.\n",
    "\t `8.`8888.          . `88888.       8 8888         `88\n",
    "\t  `8.`8888.        .8. `88888.      8 8888          88\n",
    "\t   `8.`8888.      .8`8. `88888.     8 8888          88\n",
    "\t    `8.`8888.    .8' `8. `88888.    8 8888         ,88\n",
    "\t8b   `8.`8888.  .8'   `8. `88888.   8 8888        ,88'\n",
    "\t`8b.  ;8.`8888 .888888888. `88888.  8 8888    ,o88P'  \n",
    "\t `Y8888P ,88P'.8'       `8. `88888. 8 888888888P'     \n\n"),
    # wft 42
    .p(
    "\t                ___        .-.    \n",
    "\t               (   )      /    \  \n",
    "\t ___  ___  ___  | |_      | .`. ; \n",
    "\t(   )(   )(   )(   __)    | |(___)\n",
    "\t | |  | |  | |  | |       | |_    \n",
    "\t | |  | |  | |  | | ___  (   __)  \n",
    "\t | |  | |  | |  | |(   )  | |     \n",
    "\t | |  | |  | |  | | | |   | |     \n",
    "\t | |  ; '  | |  | ' | |   | |     \n",
    "\t ' `-'   `-' '  ' `-' ;   | |     \n",
    "\t  '.__.'.__.'    `.__.   (___)    \n\n"),
    # 67
    .p(
    '\t                                                           \n',
    '\t888       888 8888888b.   .d88888b.  888b    888  .d8888b. \n',
    '\t888   o   888 888   Y88b d88P" "Y88b 8888b   888 d88P  Y88b\n',
    '\t888  d8b  888 888    888 888     888 88888b  888 888    888\n',
    '\t888 d888b 888 888   d88P 888     888 888Y88b 888 888       \n',
    '\t888d88888b888 8888888P"  888     888 888 Y88b888 888  88888\n',
    '\t88888P Y88888 888 T88b   888     888 888  Y88888 888    888\n',
    '\t8888P   Y8888 888  T88b  Y88b. .d88P 888   Y8888 Y88b  d88P\n',
    '\t888P     Y888 888   T88b  "Y88888P"  888    Y888  "Y8888P88\n\n'),
    # 44
    .p(
    '\t                                    \n',
    '\t`7MM"""Yp,      db      `7MM"""Yb.  \n',
    '\t  MM    Yb     ;MM:       MM    `Yb.\n',
    '\t  MM    dP    ,V^MM.      MM     `Mb\n',
    '\t  MM"""bg.   ,M  `MM      MM      MM\n',
    '\t  MM    `Y   AbmmmqMA     MM     ,MP\n',
    '\t  MM    ,9  A`     VML    MM    ,dP`\n',
    '\t.JMMmmmd9 .AMA.   .AMMA..JMMmmmdP`  \n\n'),
    # 66
    .p(
    '\t                                                          \n',
    '\t`7MM"""YMM  `7MM"""Mq.  `7MM"""Mq.   .g8""8q. `7MM"""Mq.  \n',
    '\t  MM    `7    MM   `MM.   MM   `MM..dP`    `YM. MM   `MM. \n',
    '\t  MM   d      MM   ,M9    MM   ,M9 dM`      `MM MM   ,M9  \n',
    '\t  MMmmMM      MMmmdM9     MMmmdM9  MM        MM MMmmdM9   \n',
    '\t  MM   Y  ,   MM  YM.     MM  YM.  MM.      ,MP MM  YM.   \n',
    '\t  MM     ,M   MM   `Mb.   MM   `Mb.`Mb.    ,dP` MM   `Mb. \n',
    '\t.JMMmmmmMMM .JMML. .JMM..JMML. .JMM. `"bmmd"` .JMML. .JMM.\n\n'),
    # 64
    .p(
    '\t                                                        \n',
    '\t8888888888 8888888b.  8888888b.   .d88888b.  8888888b.  \n',
    '\t888        888   Y88b 888   Y88b d88P" "Y88b 888   Y88b \n',
    '\t888        888    888 888    888 888     888 888    888 \n',
    '\t8888888    888   d88P 888   d88P 888     888 888   d88P \n',
    '\t888        8888888P"  8888888P"  888     888 8888888P"  \n',
    '\t888        888 T88b   888 T88b   888     888 888 T88b   \n',
    '\t888        888  T88b  888  T88b  Y88b. .d88P 888  T88b  \n',
    '\t8888888888 888   T88b 888   T88b  "Y88888P"  888   T88b \n\n'),
    # 19
    .p(
    '\t           \n',
    '\t+-+-+-+-+-+\n',
    '\t|E|R|R|O|R|\n',
    '\t+-+-+-+-+-+\n',
    '\t|W|R|O|N|G|\n',
    '\t+-+-+-+-+-+\n\n')
    )
    width = c(17,62,42,67,44,66,64,19)
    # choose
    which = rep(TRUE,length(width))
    if(!exists(".id")){which[1] = FALSE}
    which[width>getOption("width")]=FALSE
    which = which(which)
    which = which[order(runif(length(which)))][1]
    #
    warning(.p(msg[which],if(!is.null(x)).p("\t",x,"\n")),call.=FALSE)
    }

