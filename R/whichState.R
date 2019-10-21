#' whichState
#'
#' Find out which American State is the phone number from via US Area Code.
#'
#' This function has been designed to extract the first 3 digits of the provided phone number, match them to the US State Area Codes and to find out which state the phone number is from. The phone number provided has to be a proper US phone number with 10 digits.
#'
#' @param phoneNumber US Phone Number: A string of 10 digits. There should be no spacing or punctuations between digits
#' @param usa Set to TRUE by default. If usa is not true, the function cannot be executed because the US State Area codes only work for US Phone Numbers
#'
#' @author Gerald Lee
#'
#' @return The US State where the Phone Number is from
#' @keywords USA, state, phone, numbers, area, code
#' @export
#' @examples
#' whichState(1234567890, usa=TRUE)


whichState <- function(phoneNumber, usa=TRUE) {

  # Ensure that second argument is usa because function only runs for american phone numbers
  if(usa != TRUE) stop("Error: Must be a US Phone Number")

  # US State Area Codes
  alabama <- c(205, 251, 256, 334, 938)
  alaska <- c(907)
  arizona <- c(480, 520, 602, 623, 928)
  arkansas <- c(479, 501, 870)
  california <- c(209, 213, 279, 310, 323, 408, 415, 424, 442, 510, 530, 559, 562, 619, 626, 628, 650, 657, 661, 669, 707, 714, 747, 760, 805, 818, 820, 831, 858, 909, 916, 925, 949, 951)
  colorado <- c(303, 719, 720, 970)
  connecticut <- c(203, 475, 860, 959)
  delaware <- c(302)
  florida <- c(	239, 305, 321, 352, 386, 407, 561, 727, 754, 772, 786, 813, 850, 863, 904, 941, 954)
  georgia <- c(229, 404, 470, 478, 678, 706, 762, 770, 912)
  hawaii <- c(808)
  idaho <- c(208, 986)
  illinois <- c(217, 224, 309, 312, 331, 618, 630, 708, 773, 779, 815, 847, 872)
  indiana <- c(	219, 260, 317, 463, 574, 765, 812, 930)
  iowa <- c(319, 515, 563, 641, 712)
  kansas <- c(316, 620, 785, 913)
  kentucky <- c(270, 364, 502, 606, 859)
  louisiana <- c(225, 318, 337, 504, 985)
  maine <- c(207)
  maryland <- c(240, 301, 410, 443, 667)
  massachusetts <- c(339, 351, 413, 508, 617, 774, 781, 857, 978)
  michigan <- c(231, 248, 269, 313, 517, 586, 616, 734, 810, 906, 947, 989)
  minnesota <- c(218, 320, 507, 612, 651, 763, 952)
  mississippi <- c(228, 601, 662, 769)
  missouri <- c(314, 417, 573, 636, 660, 816)
  montana <- c(406)
  nebraska <- c(308, 402, 531)
  nevada <- c(702, 725, 775)
  newHampshire <- c(603)
  newJersey <- c(201, 551, 609, 640, 732, 848, 856, 862, 908, 973)
  newMexico <- c(505, 575)
  newYork <- c(212, 315, 332, 347, 516, 518, 585, 607, 631, 646, 680, 716, 718, 838, 845, 914, 917, 929, 934)
  northCarolina <- c(252, 336, 704, 743, 828, 910, 919, 980, 984)
  northDakota <- c(701)
  ohio <- c(216, 220, 234, 330, 380, 419, 440, 513, 567, 614, 740, 937)
  oklahoma <- c(405, 539, 580, 918)
  oregon <- c(458, 503, 541, 971)
  pennsylvania <- c(215, 223, 267, 272, 412, 445, 484, 570, 610, 717, 724, 814, 878)
  rhodeIsland <- c(401)
  southCarolina <- c(803, 843, 854, 864)
  southDakota <- c(605)
  tennessee <- c(423, 615, 629, 731, 865, 901, 931)
  texas <- c(210, 214, 254, 281, 325, 346, 361, 409, 430, 432, 469, 512, 682, 713, 726, 737, 806, 817, 830, 832, 903, 915, 936, 940, 956, 972, 979)
  utah <- c(385, 435, 801)
  vermont <- c(802)
  virginia <- c(276, 434, 540, 571, 703, 757, 804)
  washington <- c(206, 253, 360, 425, 509, 564)
  washingtonDC <- c(202)
  westVirginia <- c(304, 681)
  wisconsin <- c(262, 414, 534, 608, 715, 920)
  wyoming <- c(307)

  # Make sure phone number has 10 digits
  if(nchar(phoneNumber) != 10) {
    stop(print("Phone number is invalid! Make sure it has 10 digits and no punctuations / spaces in between"))
  }

  # Match the first 3 digits of Phone Number to US State Codes
  else if(substring(phoneNumber, 1, 3) %in% alabama) {
    cat("Alabama State")
  }
  else if(substring(phoneNumber, 1, 3) %in% alaska) {
    cat("Alaska State")
  }
  else if(substring(phoneNumber, 1, 3) %in% arizona) {
    cat("Arizona State")
  }
  else if(substring(phoneNumber, 1, 3) %in% arkansas) {
    cat("Arkansas State")
  }
  else if(substring(phoneNumber, 1, 3) %in% california) {
    cat("California State")
  }
  else if(substring(phoneNumber, 1, 3) %in% colorado) {
    cat("Colorado State")
  }
  else if(substring(phoneNumber, 1, 3) %in% connecticut) {
    cat("Connecticut State")
  }
  else if(substring(phoneNumber, 1, 3) %in% delaware) {
    cat("Delaware State")
  }
  else if(substring(phoneNumber, 1, 3) %in% florida) {
    cat("Florida State")
  }
  else if(substring(phoneNumber, 1, 3) %in% georgia) {
    cat("Georgia State")
  }
  else if(substring(phoneNumber, 1, 3) %in% hawaii) {
    cat("Hawaii State")
  }
  else if(substring(phoneNumber, 1, 3) %in% idaho) {
    cat("Idaho State")
  }
  else if(substring(phoneNumber, 1, 3) %in% illinois) {
    cat("Illinois State")
  }
  else if(substring(phoneNumber, 1, 3) %in% indiana) {
    cat("Indiana State")
  }
  else if(substring(phoneNumber, 1, 3) %in% iowa) {
    cat("Iowa State")
  }
  else if(substring(phoneNumber, 1, 3) %in% kansas) {
    cat("Kansas State")
  }
  else if(substring(phoneNumber, 1, 3) %in% kentucky) {
    cat("Kentucky State")
  }
  else if(substring(phoneNumber, 1, 3) %in% louisiana) {
    cat("Louisiana State")
  }
  else if(substring(phoneNumber, 1, 3) %in% maine) {
    cat("Maine State")
  }
  else if(substring(phoneNumber, 1, 3) %in% maryland) {
    cat("Maryland State")
  }
  else if(substring(phoneNumber, 1, 3) %in% massachusetts) {
    cat("Massachusetts State")
  }
  else if(substring(phoneNumber, 1, 3) %in% michigan) {
    cat("Michigan State")
  }
  else if(substring(phoneNumber, 1, 3) %in% minnesota) {
    cat("Minnesota State")
  }
  else if(substring(phoneNumber, 1, 3) %in% mississippi) {
    cat("Mississippi State")
  }
  else if(substring(phoneNumber, 1, 3) %in% missouri) {
    cat("Missouri State")
  }
  else if(substring(phoneNumber, 1, 3) %in% montana) {
    cat("Montana State")
  }
  else if(substring(phoneNumber, 1, 3) %in% nebraska) {
    cat("Nebraska State")
  }
  else if(substring(phoneNumber, 1, 3) %in% nevada) {
    cat("Nevada State")
  }
  else if(substring(phoneNumber, 1, 3) %in% newHampshire) {
    cat("New Hampshire State")
  }
  else if(substring(phoneNumber, 1, 3) %in% newJersey) {
    cat("New Jersey State")
  }
  else if(substring(phoneNumber, 1, 3) %in% newMexico) {
    cat("New Mexico State")
  }
  else if(substring(phoneNumber, 1, 3) %in% newYork) {
    cat("New York State")
  }
  else if(substring(phoneNumber, 1, 3) %in% northCarolina) {
    cat("North Carolina State")
  }
  else if(substring(phoneNumber, 1, 3) %in% northDakota) {
    cat("North Dakota State")
  }
  else if(substring(phoneNumber, 1, 3) %in% ohio) {
    cat("Ohio State")
  }
  else if(substring(phoneNumber, 1, 3) %in% oklahoma) {
    cat("Oklahoma State")
  }
  else if(substring(phoneNumber, 1, 3) %in% oregon) {
    cat("Oregon State")
  }
  else if(substring(phoneNumber, 1, 3) %in% pennsylvania) {
    cat("Pennsylvania State")
  }
  else if(substring(phoneNumber, 1, 3) %in% rhodeIsland) {
    cat("Rhode Island State")
  }
  else if(substring(phoneNumber, 1, 3) %in% southCarolina) {
    cat("South Carolina State")
  }
  else if(substring(phoneNumber, 1, 3) %in% southDakota) {
    cat("South Dakota State")
  }
  else if(substring(phoneNumber, 1, 3) %in% tennessee) {
    cat("Tennessee State")
  }
  else if(substring(phoneNumber, 1, 3) %in% texas) {
    cat("Texas State")
  }
  else if(substring(phoneNumber, 1, 3) %in% utah) {
    cat("Utah State")
  }
  else if(substring(phoneNumber, 1, 3) %in% vermont) {
    cat("Vermont State")
  }
  else if(substring(phoneNumber, 1, 3) %in% virginia) {
    cat("Virginia State")
  }
  else if(substring(phoneNumber, 1, 3) %in% washington) {
    cat("Washington State")
  }
  else if(substring(phoneNumber, 1, 3) %in% washingtonDC) {
    cat("Washington DC State")
  }
  else if(substring(phoneNumber, 1, 3) %in% westVirginia) {
    cat("West Virginia State")
  }
  else if(substring(phoneNumber, 1, 3) %in% wisconsin) {
    cat("Wisconsin State")
  }
  else if(substring(phoneNumber, 1, 3) %in% wyoming) {
    cat("Wyoming State")
  }
  else(cat("Unfortunately, we are unable to find out your state today"))
}
