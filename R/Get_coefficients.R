#' @export
Get_coefficients <- function(Family, Lmax.mean){

  if(Family=="Acanthuridae")
  {
    LinfFunc   <- "Linear"
    LinfDist   <- "Normal"
    Linf_coef  <- rbind(-39.421486,1.018236)
    Linf_cov   <- rbind(c(136.5050105, -0.345017509),
                        c( -0.3450175,  0.001037638))
    Linf_error <- 18.073072

    KFunc    <- "Expo"
    KDist    <- "Lognormal"
    K_coef   <- rbind(-0.27823534,-0.00179617)
    K_cov    <- rbind(c( 3.628268e-02,-8.492274e-05),
                      c(-8.492274e-05, 2.423078e-07))
    K_error  <- 0.41955247

    A0Dist   <- "Fixed"
    A0_coef  <- rbind(-0.6, -999, -999)
    A0_cov   <- rbind(-999)

    MFunc    <- "Linear"
    MDist    <- "Normal"
    M_coef   <- rbind(0.09768,0);
    M_cov    <- rbind(c(1.994e-5,0),c(0,0))
    M_error  <- 0.02277

    MatFunc  <- "Linear"
    MatDist  <- "Lognormal"
    Mat_coef <- rbind(-12.551248,0.827516)
    Mat_cov  <- rbind(c( 1.944283e+02,-6.928236e-01),
                      c(-6.928236e-01, 3.050065e-03))
    Mat_error<- 0.102
  }

  if(Family=="Carangidae")
  {

    if(Lmax.mean<=900){
      LinfFunc  <- "Linear"
      LinfDist  <- "Lognormal"
      Linf_coef <- rbind(27.997851,1.003558)
      Linf_cov  <- rbind(c( 1.650201e+03,-3.3814631668),
                         c(-3.3814631668, 0.0081213854))
      Linf_error<- 0.1302
    }else{
      LinfFunc  <- "Linear"
      LinfDist  <- "Lognormal"
      Linf_coef <- rbind(-27.938,1.196)
      Linf_cov  <- rbind(c(8.624730e-09,9.937220e-06),
                         c(9.937220e-06,1.145423e-02))
      Linf_error<- 0.24
    }

    KFunc    <- "Expo"
    KDist    <- "Lognormal"
    K_coef   <- rbind(-1.10386285,-0.00045998)
    K_cov    <- rbind(c( 1.772833e-02,-1.721886e-05),
                      c(-1.721886e-05, 2.206721e-08))
    K_error  <- 0.35885624

    A0Dist   <- "Fixed"
    A0_coef  <- rbind(-0.6,-999,-999)

    MFunc    <- "Power_2var"
    MDist    <- "Lognormal"
    M_coef   <- rbind(2.267174,0.030239,-0.515081)
    M_cov    <- rbind(c( 7.638689e-01,-2.137078e-03,-1.179220e-01),
                      c(-2.137078e-03, 2.197155e-02, 5.211300e-03),
                      c(-1.179220e-01, 5.211300e-03, 1.939093e-02))
    M_error  <- 0.307

    MatFunc  <- "Linear"
    MatDist  <- "Normal"
    Mat_coef <- rbind(33.576092,0.572904)
    Mat_cov  <- rbind(c(542.3375676,-0.611514575),
                      c( -0.6115146, 0.000859941))
    Mat_error<- 43.96
  }

  if(Family=="Lethrinidae")
  {

    LinfFunc  <- "Linear"
    LinfDist  <- "Normal"
    Linf_coef <- rbind(-1.441152,0.872423)
    Linf_cov  <- rbind(c(161.8062667,-0.333560243),
                       c( -0.3335602, 0.000752506))
    Linf_error<- 17.1

    KFunc    <- "Power"
    KDist    <- "Lognormal"
    K_coef   <- rbind(4.658581,-0.930113)
    K_cov    <- rbind(c( 3.124779    ,-5.257665e-01),
                      c(-5.257665e-01, 8.871238e-02))
    K_error  <- 0.438728

    A0Dist   <- "Fixed"
    A0_coef  <- rbind(-0.6,-999,-999)

    MFunc    <- "Power_2var"
    MDist    <- "Lognormal"
    M_coef   <- rbind(0.54085,0.53750,-0.28432)
    M_cov    <- rbind(c( 2.115091    , -9.768916e-02,-3.619241e-01),
                      c(-9.768916e-02,  2.494511e-02, 1.944466e-02),
                      c(-3.619241e-01,  1.944466e-02, 6.240972e-02))
    M_error  <- 0.297

    MatFunc  <- "Linear"
    MatDist  <- "Normal"
    Mat_coef <- rbind(36.013807,0.671787)
    Mat_cov  <- rbind(c(395.5265340,-1.0156028933),
                      c( -1.0156029, 0.0028603607))
    Mat_error<- 21.3
  }

  if(Family=="Lutjanidae")
  {
    if(Lmax.mean<=500){
      LinfFunc  <- "Linear"
      LinfDist  <- "Normal"
      Linf_coef <- rbind(-5.077514,0.912436)
      Linf_cov  <- rbind(c(952.051969,-2.651956402),
                         c( -2.651956, 0.007722089))
      Linf_error<- 21.3
    }else{
      LinfFunc  <- "Linear"
      LinfDist  <- "Normal"
      Linf_coef <-rbind(-81.77184,1.05954)
      Linf_cov  <-rbind(c(6711.302438,-8.30116144),
                        c(  -8.301161, 0.01076043))
      Linf_error<- 85.86606
    }

    KFunc    <- "Power"
    KDist    <- "Lognormal"
    K_coef   <- rbind(5.225192,-1.056897)
    K_cov    <- rbind(c( 7.595088e-01,-1.196958e-01),
                      c(-1.196958e-01, 1.896458e-02))
    K_error  <- 0.397114

    A0Dist   <- "Fixed"
    A0_coef  <-rbind(-0.6,-999,-999)

    MFunc    <- "Power_2var"
    MDist    <- "Lognormal"
    M_coef   <- rbind(2.0379245,-0.0086432,-0.6306451)
    M_cov    <- rbind(c( 1.723808    ,-1.386993e-01,-3.004497e-01),
                      c(-1.386993e-01, 2.507150e-02, 2.729170e-02),
                      c(-3.004497e-01, 2.729170e-02, 5.317334e-02))
    M_error  <- 0.3929

    MatFunc  <- "Linear"
    MatDist  <- "Lognormal"
    Mat_coef <- rbind(63.875705,0.519023)
    Mat_cov  <- rbind(c(423.325388756,-8.530914e-01),
                      c( -0.853091438, 2.161344e-03))
    Mat_error<- 0.148013
  }

  if(Family=="Mullidae")
  {
    LinfFunc  <- "Linear"
    LinfDist  <- "Normal"
    Linf_coef <- rbind(-9.340027,1.158293)
    Linf_cov  <- rbind(c(518.672234,-1.967218231),
                       c( -1.967218, 0.007936577))
    Linf_error<- 16.7

    KFunc    <- "Linear"
    KDist    <- "Normal"
    K_coef   <- rbind(0.466636,0)
    K_cov    <- rbind(c(0.0027,0),c(0,0))
    K_error  <- 0.1727

    A0Dist   <- "Fixed"
    A0_coef  <- rbind(-0.6,-999,-999)

    MFunc    <- "Linear"
    MDist    <- "Lognormal"
    M_coef   <- rbind(0.6744,0)
    M_cov    <- rbind(c(0.00689,0),c(0,0))
    M_error  <- 0.348

    MatFunc  <- "Linear"
    MatDist  <- "Normal"
    Mat_coef <- rbind(49.32657,0.414815)
    Mat_cov  <- rbind(c(344.831162   ,-1.135673725),
                      c( -1.135673725, 0.004357041))
    Mat_error<- 22.09
  }

  if(Family=="Scaridae")
  {
    LinfFunc  <- "Linear"
    LinfDist  <- "Normal"
    Linf_coef <- rbind(0.190650,0.849181)
    Linf_cov  <- rbind(c(378.6815365,-0.848459796),
                       c( -0.8484598, 0.002086915))
    Linf_error<- 23.2

    KFunc    <- "Expo"
    KDist    <- "Lognormal"
    K_coef   <- rbind(0.68111900,-0.00353955)
    K_cov    <- rbind(c( 2.681769e-02,-6.586431e-05),
                      c(-6.586431e-05, 1.933287e-07))
    K_error  <- 0.33085776

    A0Dist   <- "Fixed"
    A0_coef  <- rbind(-0.6,-999,-999)

    MFunc    <- "Power_2var"
    MDist    <- "Lognormal"
    M_coef   <- rbind(-0.768542,0.646086,-0.039457)
    M_cov    <- rbind(c( 3.465647    ,-2.579116e-01,-6.015445e-01),
                      c(-2.579116e-01, 3.699427e-02, 4.640341e-02),
                      c(-6.015445e-01, 4.640341e-02, 1.046714e-01))
    M_error  <- 0.241926

    MatFunc  <- "Linear"
    MatDist  <- "Normal"
    Mat_coef <- rbind(-7.967278,0.713637)
    Mat_cov  <- rbind(c(183.0398307,-0.42502341),
                      c( -0.4250234, 0.00114699))
    Mat_error<- 22
  }

  if(Family=="Shark")
  {
    LinfFunc  <- "Linear"
    LinfDist  <- "Normal"
    Linf_coef <- rbind(74.508188,1.02818)
    Linf_cov  <- rbind(c(8241.937584   ,-2.920164638),
                       c(  -2.920164638, 0.001335099))
    Linf_error<- 219.576955

    KFunc    <- "Power"
    KDist    <- "Lognormal"
    K_coef   <- rbind(5.505996,-0.9920931)
    K_cov    <- rbind(c( 1.681207 ,-0.2200081),
                      c(-0.2200081, 0.02895112))
    K_error  <- 0.4917783

    A0Dist   <- "Fixed"
    A0_coef  <- rbind(-0.6,-999,-999)

    MFunc    <- "Power_2var"
    MDist    <- "Lognormal"
    M_coef   <- rbind(3.772445,0.139914,-0.697556)
    M_cov    <- rbind(c( 0.6589237 ,-0.03649175 ,-0.09690443),
                      c(-0.03649175, 0.008354179, 0.00709543),
                      c(-0.09690443, 0.00709543 , 0.01477145))
    M_error  <- 0.267889

    MatFunc  <- "Linear"
    MatDist  <- "Lognormal"
    Mat_coef <- rbind(39.991995,0.749496)
    Mat_cov  <- rbind(c(5256.470476   ,-3.556020473),
                      c(  -3.556020473, 0.002846484))
    Mat_error<- 0.121883
  }

  if(Family=="Serranidae")
  {
    LinfFunc  <- "Linear"
    LinfDist  <- "Normal"
    Linf_coef <- rbind(16.421,0.906)
    Linf_cov  <- rbind(c(549.5694787  ,-0.586293701),
                       c( -0.586293701, 0.000840065))
    Linf_error<- 88.643

    KFunc    <- "Power"
    KDist    <- "Lognormal"
    K_coef   <- rbind(3.516075,-0.803138)
    K_cov    <- rbind(c( 0.4253909 ,-0.06669836),
                      c(-0.06669836, 0.0105578))
    K_error  <- 0.479123

    A0Dist   <- "Fixed"
    A0_coef  <- rbind(-0.6,-999,-999)

    MFunc    <- "Power_2var"
    MDist    <- "Lognormal"
    M_coef   <- rbind(0.713543,0.217078,-0.357565)
    M_cov    <- rbind(c( 0.3519742 ,-0.02449899 ,-0.06053641),
                      c(-0.02449899, 0.007134709, 0.005572682),
                      c(-0.06053641, 0.005572682, 0.01079999))
    M_error  <- 0.32839

    MatFunc  <- "Linear"
    MatDist  <- "Normal"
    Mat_coef <- rbind(61.19186,0.487651)
    Mat_cov  <- rbind(c(609.2499949   ,-0.6600851695),
                      c( -0.6600851695, 0.0008989627))
    Mat_error<- 62.136809
  }

  if(Family=="Haemulidae")
  {
    LinfFunc  <- "Linear"
    LinfDist  <- "Normal"
    Linf_coef <- rbind(-38.48345,1.08116)
    Linf_cov  <- rbind(c(2737.625959  ,-5.16729362),
                       c(  -5.16729362, 0.01111812))
    Linf_error<- 63.50251

    KFunc    <- "Power"
    KDist    <- "Lognormal"
    K_coef   <- rbind(4.215208,-0.8694549)
    K_cov    <- rbind(c( 1.856475 ,-0.3040749),
                      c(-0.3040749, 0.05001601))
    K_error  <- 0.3311729

    A0Dist   <- "Fixed"
    A0_coef  <- rbind(-0.6,-999,-999)

    MFunc    <- "Power_2var"
    MDist    <- "Lognormal"
    M_coef   <- rbind(5.812223,0.116474,-1.230305)
    M_cov    <- rbind(c( 7.3965789775,-0.443710116,-1.304127),
                      c(-0.4437101165, 0.105205181, 0.09031171),
                      c(-1.3041269954, 0.090311707, 0.2322123))
    M_error  <- 0.386752

    MatFunc  <- "Linear"
    MatDist  <- "Normal"
    Mat_coef <- rbind(106.287966,0.490394)
    Mat_cov  <- rbind(c(1416.231824,-3.094600577),
                      c(  -3.094601, 0.007869311))
    Mat_error<- 31.565864
  }

  if(Family=="Labridae")
  {
    LinfFunc  <- "Linear"
    LinfDist  <- "Normal"
    Linf_coef <- rbind(-61.939519,1.078769)
    Linf_cov  <- rbind(c(985.566489,-1.681084322),
                       c( -1.681084, 0.003810872))
    Linf_error<- 54.107324

    KFunc    <- "Power"
    KDist    <- "Lognormal"
    K_coef   <- rbind(4.1471,-0.9355539)
    K_cov    <- rbind(c( 1.542069 ,-0.2624309),
                      c(-0.2624309, 0.04526586))
    K_error  <- 0.4973524

    A0Dist   <- "Fixed"
    A0_coef  <- rbind(-0.6,-999,-999)

    MFunc    <- "Power_2var"
    MDist    <- "Lognormal"
    M_coef   <- rbind(-2.608529,0.638241,0.270727)
    M_cov    <- rbind(c( 2.698152 ,-0.2796352 ,-0.5145946),
                      c(-0.2796352, 0.05872695, 0.06001779),
                      c(-0.5145946, 0.06001779, 0.1000627))
    M_error  <- 0.419729

    MatFunc  <- "Linear"
    MatDist  <- "Lognormal"
    Mat_coef <- rbind(70.00207,0.2741)
    #Mat_cov  <- rbind(c(1128.25373293 ,-2.544039347),
    #                  c(  -2.544039347, 0.000371996))
    Mat_cov  <- rbind(c(1128.25373293 ,-2.544039),
                      c(  -2.544039, 0.007371996))
    Mat_error<- 0.27573
  }


  return( list(LinfFunc=LinfFunc,LinfDist=LinfDist,Linf_coef=Linf_coef,Linf_cov=Linf_cov,Linf_error=Linf_error,
               KFunc=KFunc,KDist=KDist,K_coef=K_coef,K_cov=K_cov,K_error=K_error,
               A0Dist=A0Dist,A0_coef=A0_coef,
               MFunc=MFunc,MDist=MDist,M_coef=M_coef,M_cov=M_cov,M_error=M_error,
               MatFunc=MatFunc,MatDist=MatDist,Mat_coef=Mat_coef,Mat_cov=Mat_cov,Mat_error=Mat_error
  )  )


} # End of function
