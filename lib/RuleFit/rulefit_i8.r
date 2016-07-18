#                                                                         
#                                                                              
#                R - procedures used with RuleFit(tm)
#     
#                              (7/11/16)                                   
#                                                                              
# Coded and copyright (2012) by Jerome H. Friedman.                            
#                                                                              
#
GetRF_HOME <- function()
{
  # Returns path to directory where rulefit.r and rf_go.exe are assumed to be stored.
  if (exists("rfhome", mode = "character")) {
    return(rfhome) 
  } else if (exists("RF_HOME", mode = "character")) {
     return(RF_HOME)
  } else {
      stop("Must have 'rfhone' or 'RF_HOME' defined")
  }
}

GetRF_WORKING_DIR <- function()
{
  # Returns path to directory to be used as working directory
  if (exists("rfhome", mode = "character")) {
    return(rfhome) 
  } else if (exists("RF_HOME", mode = "character") && exists("RF_WORKING_DIR", mode = "character")) {
     return(RF_WORKING_DIR)
  } else {
      stop("Must have 'rfhone' or 'RF_HOME' and 'RF_WORKING_DIR' defined")
  }
}

rfversion=function() {                                                       
   cat(" R/RuleFit3 beta (7/7/12).\n")
   cat(" Copyright (2012) by Jerome H. Friedman.\n")                           
   invisible()                                                                 
}                                                                              
rulefit=function (x,y,wt=rep(1,n),cat.vars=NULL,not.used=NULL,
   xmiss=9.0e30, rfmode="regress",sparse=1,
   test.reps=round(min(20,max(0.0,5200/neff-2))),test.fract=0.2,
   mod.sel=2,model.type='both',tree.size=4,max.rules=2000,max.trms=500,
   costs=c(1,1),trim.qntl=0.025,samp.fract=min(1,(11*sqrt(neff)+1)/neff),
   inter.supp=3.0,memory.par=0.01,conv.thr=1.0e-3,quiet=F,
   tree.store=10000000,cat.store=1000000) {
   thecall=match.call()
   if(!is.logical(quiet)) { quiet=F
      warning('quiet must be logical (T/F). F substituted.')
   }
   if (!is.character(rfmode)) stop("rfmode must be of type character.")    
   if(rfmode == "regress") {mode=1}
   else if (rfmode=="class") {mode=2}
   else {                          
      stop(paste("rfmode must equal either 'regress' for regression,",       
      "or 'class' for classification."))
   }
   if (!is.character(model.type))
      stop(" model.type must be of type character.")
   if(model.type=='linear') { lmod=0}
   else if(model.type=='rules') { lmod=1}
   else if(model.type=='both') { lmod=2}
   else { lmod=2
      warning(' improper value for model.type. model.type="both" used.')
   }   
   if(model.type=='rules') { mtrm=max.rules}
   else { mtrm=max.rules+ncol(x)}
   if(sparse<=0) {
      if(max.trms < mtrm) { max.trms=mtrm
       warning(paste('sparse=0 (ridge): max.trms reset to',as.character(mtrm)))
      }
   }
   if(is.data.frame(x)) { p=length(x)}
   else if(is.matrix(x)) { p <- ncol(x)}
   else { stop(" x must be a matrix or data frame.")}
   if(is.null(colnames(x))) { varnames=paste('V',as.character(1:p),sep='')}
   else { varnames=colnames(x)}
   if(is.data.frame(x)) { n=length(x[[1]])
      xx=matrix(nrow=n,ncol=p); for (j in 1:p) { xx[,j]=x[[j]]}
      if(is.null(cat.vars)) { lx=as.numeric(sapply(x,is.factor))+1}
      else { lx=rep(1,p)
         iv=getvars(cat.vars,p,lx,varnames); iv=iv[iv!=0]
         if(length(iv)!=0) lx[iv]=2
      }
   }
   else { n <- nrow(x);  xx=x; lx=rep(1,p)
      if(!is.null(cat.vars)) {
         iv=getvars(cat.vars,p,lx,varnames); iv=iv[iv!=0]
         if(length(iv)!=0) lx[iv]=2
      }         
   }
   if(length(y)==1) {
      iv=getvars(y,p,lx,varnames)
      if(iv!=0) {yy=xx[,iv]; lx[iv]=0} else {stop()}
   }
   else {yy=y}
   if(length(yy)!=n) stop (" x and y dimensions inconsistent.")
   if(max(yy)==min(yy)) stop('all y values are the same.')
   if(mode==2) {u=unique(yy); ue=0
      if(length(u)!=2) {ue=1}
      else if((u[1]!=-1 && u[1]!=1) || (u[2]!=-1 && u[2]!=1)) {ue=1}
      if(ue!=0) stop('y must be in {-1,1} for classification.')
   }
   if(length(wt)==1) {
      iv=getvars(wt,p,lx,varnames)
      if(iv!=0) {wtt=xx[,iv]; lx[iv]=0} else {stop()}
   }
   else {wtt=wt}
   if(length(wtt)!=n) stop (" x and wt dimensions inconsistent.")  
   if(!is.null(not.used)) {
      iv=getvars(not.used,p,rep(1,p),varnames); iv=iv[iv!=0]
      if(length(iv)!=0) lx[iv]=0
   }
   if (all(lx == 0)) stop("all predictor variables excluded.")
   xx[is.na(xx)] <- xmiss   
   bgstx=max(xx[xx!=xmiss])
   if(xmiss<=bgstx)
      stop(paste('value of xmiss =',xmiss,
      'is smaller than largest predictor variable value =',bgstx))
   if(any(is.na(yy))) { wtt[is.na(yy)] <- 0;
      warning(" response contains NA's - corresponding weights set to 0.")
   }
   if(any(is.na(wtt))) { wtt[is.na(wtt)] <- 0;                              
      warning(" weights contain NA's - zeros substituted.")
   }                                                                        
   if(any(wtt<0)) { wtt[wtt<0] <- 0;
      warning(" weights contain negative numbers - zeros substituted.")
   }
   neff=sum(wtt)**2/sum(wtt**2)
   if (mode==2) { beta=sum(wtt[yy < 0])/sum(wtt)
      neff=4.0*beta*(1.0-beta)*neff
   }
   if (neff < 40) {
      if (lmod > 0) { lmod=0
         warning(paste('minimum effective sample size for rules = 40.',
          'model.type reset to "linear"'))
      }
   }      
   tree.size <- parchk("tree.size",tree.size,2,n,4)                
   memory.par <- parchk("memory.par",memory.par,0.0,1.0,0.01)
   samp.fract <- parchk("samp.fract",samp.fract,1.0e-14,1.0,
      min(1,(11*sqrt(neff)+1)/neff))                   
   trim.qntl <- parchk("trim.qntl",trim.qntl,0.0,0.5,0.025)
   max.rules <- parchk("max.rules",max.rules,100,100000,2000)
   inter.supp <- parchk("inter.supp",inter.supp,1.0,100.0,3.0)
   test.fract <- parchk("test.fract",test.fract,0.1,0.5,0.2)
   test.reps <- parchk("test.reps ",test.reps,0,100,
      round(min(100,max(0.0,5200/neff-2))))
   sparse <- parchk("sparse",sparse,0,3,1)
   mod.sel <- parchk("mod.sel",mod.sel,1,3,2)
   max.trms <- parchk("max.trms",max.trms,1,mtrm,min(mtrm,500))
   conv.thr <- parchk("conv.thr",conv.thr,1.0e-5,1.0e-2,1.0e-3)
   tree.store <- parchk("tree.store",tree.store,10000,10000000,1000000)
   cat.store <- parchk("cat.store",cat.store,10000,10000000,100000)

   # If in "batch" mode, Log parameter values
   if (exists("RF_HOME", mode = "character") && exists("RF_WORKING_DIR", mode = "character")) {
     opt.str <- sprintf("x (dim = %d x %d), y, wt, cat.vars: %s, not.used: %s, xmiss: %e, rfmode: %s, sparse: %.2f, test.reps: %d, test.fract: %f, mod.sel: %d, model.type: %s, tree.size: %d, max.rules: %d, max.trms: %d, costs: %s, trim.qntl: %f, samp.fract: %f, inter.supp: %f, memory.par: %f, conv.thr: %f, tree.store: %d, cat.store: %d",
                      nrow(x), ncol(x),
                      paste(cat.vars, collapse = ","), paste(not.used, collapse = ","),
                      xmiss, rfmode, sparse, test.reps, test.fract, mod.sel, model.type,
                      tree.size, max.rules, max.trms, paste(costs, collapse = ","), trim.qntl,
                      samp.fract, inter.supp, memory.par, conv.thr, tree.store, cat.store)
     info(logger, paste("RuleFit received options:", opt.str))
     dbg(logger, paste("RuleFit received columns:", paste(varnames, collapse=", ")))
   }
   
   intp=c(mode,lmod,n,p,max.rules,tree.size,test.reps,mod.sel,
      max.trms,tree.store,cat.store)
   rlp=c(xmiss,trim.qntl,test.fract,inter.supp,memory.par,samp.fract,
      costs,conv.thr,sparse)
   zz=file(paste(GetRF_WORKING_DIR(),'/intparms',sep=''),'wb')
   writeBin(as.integer(intp),zz,size=8); close(zz)
   zz=file(paste(GetRF_WORKING_DIR(),'/realparms',sep=''),'wb')                            
   writeBin(as.double(rlp),zz,size=8); close(zz)                                 
   write(varnames,file=paste(GetRF_WORKING_DIR(),'/varnames',sep=''))                 
   zz=file(paste(GetRF_WORKING_DIR(),'/lx',sep=''),'wb')                                
   writeBin(as.integer(lx),zz,size=8); close(zz)                            
   zz=file(paste(GetRF_WORKING_DIR(),'/train.x',sep=''),'wb')
   writeBin(as.double(as.vector(xx)),zz,size=8); close(zz)  
   zz=file(paste(GetRF_WORKING_DIR(),'/train.y',sep=''),'wb')                              
   writeBin(as.double(yy),zz,size=8); close(zz)                             
   zz=file(paste(GetRF_WORKING_DIR(),'/train.w',sep=''),'wb')
   writeBin(as.double(wtt),zz,size=8); close(zz)
   wd=getwd(); setwd(GetRF_WORKING_DIR())
   status=rfexe('rulefit',minimized=quiet)
   setwd(wd)                        
   if(status!='OK') { rfstat()
      cat('no RuleFit model produced.\n'); stop()
   }
   read.file('rfout')
   save(thecall,file=paste(GetRF_WORKING_DIR(),'/thecall',sep='')) 
   invisible(getmodel())
}                                                                              
rfpred=function (x) {
   if(is.data.frame(x)) { p=length(x); n=length(x[[1]])
      xx=matrix(nrow=n,ncol=p); for (j in 1:p) { xx[,j]=x[[j]]}
   }
   else if(is.matrix(x)) { n <- nrow(x); p <- ncol(x); xx=x;}
   else if(is.vector(x)) { p=length(x); n=1; xx=x}
   else { stop(' x must be a data frame, matrix, or vector.')}
   zz=file(paste(GetRF_WORKING_DIR(),'/rulefit.mod',sep=''),'rb')
   info=readBin(zz,integer(),size=8,n=4); close(zz)
   if (p!=info[4]) {                                                     
      stop(paste(" number of variables =",as.character(p),                     
         " is different from training data =",as.character(info[4])))      
   }
   if(any(is.na(xx))) { 
      zz=file(paste(GetRF_WORKING_DIR(),'/realparms',sep=''),'rb')
      xmiss=readBin(zz,numeric(),size=8,n=1); close(zz)
      xx[is.na(xx)] <- xmiss;
   }                                                                           
   zz=file(paste(GetRF_WORKING_DIR(),'/test.x',sep=''),'wb')
   writeBin(as.numeric(c(n,as.vector(xx))),zz,size=8); close(zz)
   wd=getwd(); setwd(GetRF_WORKING_DIR())
   status=rfexe('rulefit_pred')
   setwd(wd)
   if(status!='OK') { rfstat(); stop()}
   zz=file(paste(GetRF_WORKING_DIR(),'/yhat',sep=''),'rb')
   info=readBin(zz,numeric(),size=8,n=1); close(zz)
   zz=file(paste(GetRF_WORKING_DIR(),'/yhat',sep=''),'rb')
   yp=readBin(zz,numeric(),size=8,n=info[1]+1); close(zz)   
   yp[2:(info[1]+1)]
}                                                                              
parchk <- function (ax,x,lx,ux,df) {                                           
   if(x < lx || x > ux) {                                                   
      warning(paste(" invalid value for",ax,"- default (",df,") used."))  
      df     
   }                                                                           
   else { x}                                                                   
}                                                                              
read.file=function(file='rfhelp.hlp') {
   wd=getwd(); setwd(GetRF_WORKING_DIR())
   if (platform=='windows') {
      write('readfile',file=paste(GetRF_WORKING_DIR(),'/program',sep=''))
      system(file.path(GetRF_HOME(),'rf_go.exe'),input=file,show.output.on.console=T,minimized=T)
   }
   else { system(paste('cat ',GetRF_WORKING_DIR(),'/',file,sep=''))}
   setwd(wd)
}                                                                              
rfstat <- function() {                                                       
   read.file('rfstatus')                                              
   invisible()                                                                 
}                                                                              
rfhelp <- function(fun="rfhelp") {
   if (!is.character(fun)) stop("argument must be of type character.")         
   wd=getwd(); setwd(GetRF_WORKING_DIR())
   if (platform=='windows'){
      system(paste('cmd /k type ',fun,'.hlp',sep=''),invisible=F,
         show.output.on.console=F,wait=T)
   }
   else { system(paste('xterm -sb -sl 500 -hold -T ',fun,
      ' -rightbar -e cat ',fun,'.hlp&',sep=''))}
   setwd(wd)
   invisible()                                                                 
}
getmodel=function() {
   model=list();
   model[[1]]=scan(paste(GetRF_WORKING_DIR(),'/rfout',sep=''),what='',quiet=T)
   zz=file(paste(GetRF_WORKING_DIR(),'/rulefit.mod',sep=''),'rb')
   info=readBin(zz,integer(),size=8,n=6); close(zz)
   len=10+15*info[1]+2*info[2]+2*info[3]+12*info[6]
   zz=file(paste(GetRF_WORKING_DIR(),'/rulefit.mod',sep=''),'rb')
   model[[2]]=readBin(zz,what="raw",n=(8*len)); close(zz)
   zz=file(paste(GetRF_WORKING_DIR(),'/rulefit.sum',sep=''),'rb')
   info=readBin(zz,integer(),size=8,n=5); close(zz)
   len=10+10*info[1]+6*info[2]+2*info[3]+12*info[5]
   zz=file(paste(GetRF_WORKING_DIR(),'/rulefit.sum',sep=''),'rb')
   model[[3]]=readBin(zz,what="raw",n=(8*len)); close(zz)
   zz=file(paste(GetRF_WORKING_DIR(),'/intparms',sep=''),'rb')
   model[[4]]=readBin(zz,integer(),size=8,n=12); close(zz)
   zz=file(paste(GetRF_WORKING_DIR(),'/realparms',sep=''),'rb')
   model[[5]]=readBin(zz,numeric(),size=8,n=10); close(zz)
   model[[6]]=scan(paste(GetRF_WORKING_DIR(),'/varnames',sep=''),what='',quiet=T)
   zz=file(paste(GetRF_WORKING_DIR(),'/lx',sep=''),'rb')
   model[[7]]=readBin(zz,integer(),size=8,n=model[[4]][4]); close(zz)
   model[[8]]=check.data(model[[4]][3],model[[4]][4])
   load(paste(GetRF_WORKING_DIR(),'/thecall',sep=''))
   model[[9]]=thecall
   invisible(model)
}
putmodel=function(model) {
   if(!is.list(model)) stop(' input model must be a list.')
   if(model[[1]][1]!='RuleFit') stop('model not from RuleFit')
   writerfout(model[[1]],1)
   zz=file(paste(GetRF_WORKING_DIR(),'/rulefit.mod',sep=''),'wb')
   writeBin(model[[2]],zz); close(zz); 
   zz=file(paste(GetRF_WORKING_DIR(),'/rulefit.sum',sep=''),'wb')
   writeBin(model[[3]],zz); close(zz);
   zz=file(paste(GetRF_WORKING_DIR(),'/intparms',sep=''),'wb')
   writeBin(as.integer(model[[4]]),zz,size=8); close(zz)
   zz=file(paste(GetRF_WORKING_DIR(),'/realparms',sep=''),'wb')
   writeBin(as.double(model[[5]]),zz,size=8); close(zz)
   write(model[[6]],file=paste(GetRF_WORKING_DIR(),'/varnames',sep=''))
   zz=file(paste(GetRF_WORKING_DIR(),'/lx',sep=''),'wb')
   writeBin(as.integer(model[[7]]),zz,size=8); close(zz)
   if(length(model)>8) { thecall=model[[9]]; #print(thecall,quote=F)
      save(thecall,file=paste(GetRF_WORKING_DIR(),'/thecall',sep=''))
   }
   read.file('rfout')
   invisible()
}
writerfout=function(out,do) {
   wout=rep(' ',3)
   wout[1]=paste(out[1:4],collapse=' ')
   b1=' '; b2='         '; b3='           '
   wout[2]=paste(out[5],b1,out[6],b1,out[7],b2,out[8],b3,
      out[9],b1,out[10],sep='')
   b1='             '; b2='            '; b3='            '   
   wout[3]=paste(b1,out[11],b2,out[12],b3,out[13],sep='')
   if(do==1) {write(wout,file=paste(GetRF_WORKING_DIR(),'/rfout',sep=''))}
   else {
      cat(wout[1]); cat('\n')
      cat(wout[2]); cat('\n')
      cat(wout[3]); cat('\n')
   }
   invisible()
}
singleplot=function(vars,qntl=0.025,nval=200,nav=500,catvals=NULL,
   samescale=F,las=2,horiz=F,cex.names=1,col='cyan',denqnt=0.1) {
   zz=file(paste(GetRF_WORKING_DIR(),'/intparms',sep=''),'rb')
   ip=readBin(zz,integer(),size=8,n=4); close(zz)
   qntl <- parchk("qntl",qntl,0.0,0.5,0.025)
   nval=min(nval,ip[3]); nav=min(nav,ip[3])
   nval <- parchk("nval",nval,10,ip[3],200)
   nav <- parchk("nav",nav,10,ip[3],500)
   las <- parchk("las",las,1,2,2)
   if(!is.logical(samescale)) { samescale=F
      warning('samescale must be logical (T/F). F substituted.')
   }
   if(!is.character(col)) { col='grey'
      warning('col must be character. "grey" substituted.')
   }
   zz=file(paste(GetRF_WORKING_DIR(),'/realparms',sep=''),'rb')
   xmiss=readBin(zz,numeric(),size=8,n=1); close(zz)
   n <- ip[3]; p <- ip[4]
   zz=file(paste(GetRF_WORKING_DIR(),'/lx',sep=''),'rb')
   lx=readBin(zz,integer(),size=8,n=ip[4]); close(zz)
   names=scan(paste(GetRF_WORKING_DIR(),'/varnames',sep=''),what='',quiet=T)
   iv=getvars(vars,p,lx,names)
   iv=iv[iv!=0]; nplot=length(iv)
   if(nplot==0) stop(' no valid plot variables.')
   iv=c(nval,nav,nplot,iv)
   zz=file(paste(GetRF_WORKING_DIR(),'/intsingleparms',sep=''),'wb')
   writeBin(as.integer(iv),zz,size=8); close(zz)
   zz=file(paste(GetRF_WORKING_DIR(),'/realsingleparms',sep=''),'wb')
   writeBin(as.numeric(qntl),zz,size=8); close(zz)
   wd=getwd(); setwd(GetRF_WORKING_DIR())
   status=rfexe('singleplot')
   setwd(wd)
   if(status!='OK') {rfstat(); stop()}
   else {
      zz=file(paste(GetRF_WORKING_DIR(),'/singleplot',sep=''),'rb')
      lo=readBin(zz,numeric(),size=8,n=1); close(zz)
      zz=file(paste(GetRF_WORKING_DIR(),'/singleplot',sep=''),'rb')
      z=readBin(zz,numeric(),size=8,n=lo); close(zz)
      if (nplot==1) { ir=1; nc=1}
      else { nc=max(2,as.integer(sqrt(nplot)))
         ir=trunc(nplot/nc);  if (nc*ir!=nplot) {ir=ir+1}
         oldpar=par(mfrow=c(ir,nc)); on.exit(par(oldpar))
      }
      nms=z[2:(nplot+1)]; nv=z[(nplot+2):(2*nplot+1)]; l=2*nplot+2;
      yl='Partial dependence'; ncat=0
      if (samescale) { yvm=-9.9e35
         for (k in 1:nplot) {
            if (nv[k] > 0) { l=l+nv[k]
               yv=z[l:(nv[k]+l-1)]; l=l+nv[k]
               yv=yv-min(yv); yvm=max(yvm,yv)
            }
         }
         l=2*nplot+2;
      }
      if (denqnt > 0) {
         zz=file(paste(GetRF_WORKING_DIR(),'/train.x',sep=''),'rb')
         data=readBin(zz,numeric(),size=8,n=n*p); close(zz)
         data=matrix(data,nrow=n,ncol=p)
      }
      for (k in 1:nplot) {
         if (nv[k] > 0) {
            xv=z[l:(nv[k]+l-1)]; l=l+nv[k]
            yv=z[l:(nv[k]+l-1)]; l=l+nv[k]
            yv=yv-min(yv); xl=names[nms[k]]
            if (!samescale) yvm=max(yv)
            if (lx[nms[k]]==1) {
               plot(xv,yv,type='l',ylim=c(0,yvm),ylab=yl,xlab=xl)
               if (denqnt>0) {
                  q1=quantile(data[,nms[k]],seq(0,1,denqnt)[2:(1/denqnt)],na.rm=T)
                  q2=q1; dlxv=0.01*(max(xv)-min(xv))
                  for (i in 2:length(q1)) { 
                     if(q1[i]==q1[i-1]) q2[i]=q1[i]+dlxv*runif(1)                 
                  }
                  axis(3,q2,labels=F)
               }
            }
            else { ncat=ncat+1
               if (ncat==1 && !missing(catvals)) { ln=length(xv); xnames=xv
                  for (j in 1:ln) {
                     if (xv[j] >= xmiss) { xnames[j]='M'}
                     else { xnames[j]=catvals[j]}
                  }
               }
               else { xv[xv>=xmiss]='M'; xnames=as.character(xv)}
               if (horiz) {
                  barplot(yv,names=xnames,xlim=c(0,yvm),xlab=yl,las=1,col=col,
                     horiz=T,cex.names=cex.names)
               }
               else {
                  barplot(yv,names=xnames,ylim=c(0,yvm),ylab=yl,las=las,
                     col=col,cex.names=cex.names)
               }
            }
            title(xl)
         }
      }
   }
   invisible()
}
getvars=function(vars,p,lx,names) {
   lv=length(vars); iv=rep(0,lv);    
   if(!is.character(vars)) {
      for (j in 1:lv) {
         if(vars[j]<1 || vars[j]>p || lx[vars[j]]==0) {
           stop(paste(vars[j],"is not one of the input variables."))
         }
         else { iv[j]=vars[j]}
      }
   }
   else {
      for (j in 1:lv) {  k=(1:p)[names==vars[j]];
         if (length(k)>0) { if(lx[k]>0) { iv[j]=k}}
         if(iv[j]==0) {
            stop(paste(names[lx>0],collapse=' '),'\n',vars[j],
               " is not one of the above input variables.")
         }
      }
   }
   iv
}
rfrestore=function(model,x=NULL,y=NULL,wt=rep(1,n)) {
   putmodel(model)
   zz=file(paste(GetRF_WORKING_DIR(),'/realparms',sep=''),'rb')
   xmiss=readBin(zz,numeric(),size=8,n=1); close(zz)
   if(!missing(x)) {   
      if(is.data.frame(x)) { p=length(x); n=length(x[[1]])
         xx=matrix(nrow=n,ncol=p); for (j in 1:p) { xx[,j]=x[[j]]}
      }
      else if(is.matrix(x)) { n <- nrow(x); p <- ncol(x); xx=x;}
      else { stop(" x must be a matrix or data frame.")}
      xx[is.na(xx)] <- xmiss;
      zz=file(paste(GetRF_WORKING_DIR(),'/train.x',sep=''),'wb')
      writeBin(as.double(as.vector(xx)),zz,size=8); close(zz)
      zz=file(paste(GetRF_WORKING_DIR(),'/intparms',sep=''),'rb')
      mode=readBin(zz,integer(),size=8,n=1); close(zz)
   }
   else {
      zz=file(paste(GetRF_WORKING_DIR(),'/intparms',sep=''),'rb')
      ip=readBin(zz,integer(),size=8,n=4); close(zz)
      n=ip[3]; p=ip[4]; mode=ip[1]
   }
   if (!missing(y) || !missing(wt)) {
      if(length(y)==1 || length(wt)==1) {
         zz=file(paste(GetRF_WORKING_DIR(),'/train.x',sep=''),'rb')
         xx=readBin(zz,numeric(),size=8,n=n*p); close(zz)
         xx=matrix(xx,nrow=n,ncol=p)
      }
   }
   if(!missing(wt)) putw(wt,xx,n,p)
   if(!missing(y)) {
      if(length(y)==1) {
         varnames=scan(paste(GetRF_WORKING_DIR(),'/varnames',sep=''),what='',quiet=T)
         iv=getvars(y,p,rep(1,p),varnames);
         if(iv!=0) { yy=xx[,iv]} else {stop()}
      }
      else {yy=y}
      if(length(yy)!=n) stop (" x and y dimensions inconsistent.")
      if(mode==2) {u=unique(yy); ue=0
         if(length(u)!=2) {ue=1}
         else if((u[1]!=-1 && u[1]!=1) || (u[2]!=-1 && u[2]!=1)) {ue=1}
         if(ue!=0) stop('y must be in {-1,1} for classification.')
      }
      zz=file(paste(GetRF_WORKING_DIR(),'/train.y',sep=''),'wb') 
      writeBin(as.double(yy),zz,size=8); close(zz)
   }
   if(!valid.data(model[[8]],n,p)) stop('model inconsistent with input data.')
   invisible()
}
putw=function(wt,xx,n,p,file='train.w') {
   if(length(wt)==1 && n>1) {
      varnames=scan(paste(GetRF_WORKING_DIR(),'/varnames',sep=''),what='',quiet=T)
      iv=getvars(wt,p,rep(1,p),varnames);
      if(iv!=0) {wtt=xx[,iv]} else {stop()}
   }
   else {wtt=wt}
   if(length(wtt)!=n) stop (" x and wt dimensions inconsistent.")
   if(any(is.na(wtt))) { wtt[is.na(wtt)] <- 0;
      warning(" weights contain NA's - zeros substituted.")
   }
   if(any(wtt<0)) { wtt[wtt<0] <- 0;
      warning(" weights contain negative numbers - zeros substituted.")
   }
   zz=file(paste(GetRF_WORKING_DIR(),'/',file,sep=''),'wb')
   writeBin(as.double(wtt),zz,size=8); close(zz)
   invisible()
}
pairplot=function(var1,var2,type='image',chgvars=F,
   qntl=0.025,nval=200,nav=500,vals1=NULL,vals2=NULL,
   theta=30,phi=15,col='cyan',las=2,horiz=F,cex.names=1) {
   zz=file(paste(GetRF_WORKING_DIR(),'/intparms',sep=''),'rb')
   ip=readBin(zz,integer(),size=8,n=4); close(zz)
   qntl <- parchk("qntl",qntl,0.0,0.5,0.025)
   nval=min(nval,ip[3]); nav=min(nav,ip[3])
   nval <- parchk("nval",nval,10,ip[3],200)
   nav <- parchk("nav",nav,10,ip[3],500)
   las <- parchk("las",las,1,2,2)
   if(!is.logical(chgvars)) { chgvars=F
      warning('chgvars must be logical (T/F). F substituted.')
   }
   if(!is.character(type)) { type='image'
      warning('type must be character. "image" substituted.')
   }
   if(!is.character(col)) { col='cyan'
      warning('type must be character. "cyan" substituted.')
   }
   zz=file(paste(GetRF_WORKING_DIR(),'/realparms',sep=''),'rb')
   xmiss=readBin(zz,numeric(),size=8,n=1); close(zz)
   n <- ip[3]; p <- ip[4]
   zz=file(paste(GetRF_WORKING_DIR(),'/lx',sep=''),'rb')
   lx=readBin(zz,integer(),size=8,n=ip[4]); close(zz)
   names=scan(paste(GetRF_WORKING_DIR(),'/varnames',sep=''),what='',quiet=T)
   v1=getvars(var1,p,lx,names); v2=getvars(var2,p,lx,names)
   if(v1==0 || v2==0) stop('plot variables not valid.')
   if(v1==v2) stop('specified plotting variables must not be the same.')
   zz=file(paste(GetRF_WORKING_DIR(),'/intpairparms',sep=''),'wb')
   writeBin(as.integer(c(nval,nav,v1,v2)),zz,size=8); close(zz)
   zz=file(paste(GetRF_WORKING_DIR(),'/realpairparms',sep=''),'wb')
   writeBin(as.numeric(qntl),zz,size=8); close(zz)
   wd=getwd(); setwd(GetRF_WORKING_DIR())
   status=rfexe('pairplot')
   setwd(wd)
   if(status!='OK') {rfstat(); stop()}
   else {
      zz=file(paste(GetRF_WORKING_DIR(),'/pairplot',sep=''),'rb')
      lo=readBin(zz,numeric(),size=8,n=1); close(zz)
      zz=file(paste(GetRF_WORKING_DIR(),'/pairplot',sep=''),'rb')
      z=readBin(zz,numeric(),size=8,n=lo); close(zz)
      nms=z[2:3]; nv=z[4]; l=5; ybl='Partial dependence'
      xv1=z[l:(nv+l-1)]; l=l+nv; xv2=z[l:(nv+l-1)]; l=l+nv
      yv=z[l:(nv+l-1)]; l=l+nv; yv=yv-min(yv);
      xl=names[nms[1]]; yl=names[nms[2]]
      if (lx[nms[1]]==1 && lx[nms[2]]==1) {
         h=interp(xv1,xv2,yv)
         if (type=='image') { image(h,xlab=xl,ylab=yl)}
         else if (type=='contour') { contour(h,xlab=xl,ylab=yl)}
         else {
            persp(h,zlab=ybl,xlab=xl,ylab=yl,theta=theta,phi=phi,
               col=col,ticktype='detailed',shade=0.5,ltheta=theta,lphi=phi)
         }
         title(ybl)
      }
      else {
         if (lx[nms[1]]==1 && lx[nms[2]]==2) {
            multplot(yv,xv1,xv2,1,xl,yl,ybl,xmiss,las,vals1,vals2,
               col,horiz,cex.names)
         }
         if (lx[nms[1]]==2 && lx[nms[2]]==1) {
            multplot(yv,xv2,xv1,1,yl,xl,ybl,xmiss,las,vals2,vals1,
               col,horiz,cex.names)
         }
         if (lx[nms[1]]==2 && lx[nms[2]]==2) {
            ll=length(unique(xv1)) > length(unique(xv2))
            if (ll && !chgvars || !ll && chgvars) {
               multplot(yv,xv1,xv2,2,xl,yl,ybl,xmiss,las,vals1,vals2,
                  col,horiz,cex.names)
            }
            else {
              multplot(yv,xv2,xv1,2,yl,xl,ybl,xmiss,las,vals2,vals1,
                 col,horiz,cex.names)
            }
         }
      }
   }
   invisible()
}
multplot=function(y,x1,x2,lx1,v1,v2,ybl,xmiss,las,vls1,vls2,col,
   horiz,cex.names) {
   vals=sort(unique(x2)); lvs=length(vals);
   nc=max(2,as.integer(sqrt(lvs)))
   ir=trunc(lvs/nc);  if (nc*ir!=lvs) {ir=ir+1}
   oldpar=par(mfrow=c(ir,nc)); on.exit(par(oldpar));
   maxyy=-9.9e40
   for (i in 1:lvs) { yy=y[x2==vals[i]]; yy=yy-min(yy); maxyy=max(maxyy,yy)}
   if (lx1 == 1) { minx=min(x1); maxx=max(x1);
      for (i in 1:lvs) {
         xx=x1[x2==vals[i]]; yy=y[x2==vals[i]]; yy=yy-min(yy); o=order(xx)
         plot(xx[o],yy[o],ylim=c(0,maxyy),xlim=c(minx,maxx),ylab=ybl,
            type='l',xlab=v1)
         if (vals[i] >= xmiss) { u='M'}
         else {
           if (is.null(vls2)) { u=as.character(vals[i])} else { u=vls2[i]}
         }
         title(paste(v2,' = ',u))
      }
   }
   else { ux1=sort(unique(x1)); ln1=length(ux1)
      for (i in 1:lvs) { xx=x1[x2==vals[i]]; yy=y[x2==vals[i]]; yy=yy-min(yy);
         if (!is.null(vls1)) { ln=length(xx); xnames=xx
            for (j in 1:ln) {
               if (xx[j] >= xmiss) { xnames[j]='M'}
               else { k=(1:ln1)[ux1=xx[j]]; xnames[j]=vls1[k]}
            }
         }
         else { xx[xx>=xmiss]='M'; xnames=as.character(xx)}
         if (horiz) {
            barplot(yy,names=xnames,xlim=c(0,maxyy),col=col,xlab=ybl,ylab=v1,
               horiz=T,las=1,cex.names=cex.names)
         }
         else {
            barplot(yy,names=xnames,ylim=c(0,maxyy),col=col,
               ylab=ybl,xlab=v1,las=las,cex.names=cex.names)
         }
         if (vals[i] >= xmiss) { u='M'}
         else {
           if (is.null(vls2)) { u=as.character(vals[i])} else { u=vls2[i]}
         }
         title(paste(v2,' = ',u))
      }
   }
   invisible()
}
varimp=function(range=NULL,impord=T,x=NULL,wt=rep(1,n),rth=0.0,plot=T,
   horiz=F,cex.names=1,col='grey',donames=T,las=2) {
   rth <- parchk("rth",rth,0.0,1.0,0.0)
   if(!is.logical(plot)) { plot=T
      warning('plot must be logical (T/F). T substituted.')
   }
   if(!is.logical(impord)) { impord=T
      warning('impord must be logical (T/F). T substituted.')
   }
   if(!is.logical(donames)) { donames=T
      warning('donames must be logical (T/F). T substituted.')
   }
   if(!is.character(col)) { col='grey'
      warning('col must be character. "grey" substituted.')
   }
   zz=file(paste(GetRF_WORKING_DIR(),'/intparms',sep=''),'rb')
   ip=readBin(zz,integer(),size=8,n=4); close(zz); ni=ip[4]
   if(!missing(x)) { mode=1
      if(is.data.frame(x)) { p=length(x); n=length(x[[1]])
         xx=matrix(nrow=n,ncol=p); for (j in 1:p) { xx[,j]=x[[j]]}
      }
      else if(is.matrix(x)) { n <- nrow(x); p <- ncol(x); xx=x;}
      else if(is.vector(x)) { p=length(x); n=1; xx=x}
      else { stop(' x must be a data frame, matrix, or vector.')}
      if(p!=ni)
         stop(paste(' number of variables =',as.character(p),
            'is different from training data =',as.character(ni)))
      if(any(is.na(xx))) {
         zz=file(paste(GetRF_WORKING_DIR(),'/realparms',sep=''),'rb')
         xmiss=readBin(zz,numeric(),size=8,n=1); close(zz)
         xx[is.na(xx)] <- xmiss;
      }
      zz=file(paste(GetRF_WORKING_DIR(),'/varimp.x',sep=''),'wb')
      writeBin(as.numeric(c(n,as.vector(xx))),zz,size=8); close(zz)
      putw(wt,xx,n,p,'varimp.w')
   }
   else { mode=0}
   zz=file(paste(GetRF_WORKING_DIR(),'/realvarimp',sep=''),'wb')
   writeBin(as.numeric(c(mode,rth)),zz,size=8); close(zz)
   wd=getwd(); setwd(GetRF_WORKING_DIR())
   status=rfexe('varimp')
   setwd(wd)
   if(status!='OK') {rfstat(); stop()}
   else {   
      zz=file(paste(GetRF_WORKING_DIR(),'/varimp',sep=''),'rb')
      vi=readBin(zz,numeric(),size=8,n=2*ni); close(zz)
      vi[1:ni]=100*vi[1:ni]/max(vi[1:ni]); o=vi[(ni+1):(2*ni)]; vii=vi[1:ni]
      if(!impord) {
         for (i in 1:ni) { vii[o[i]]=vi[i]}
         o=1:ni
      }
      if(plot) {
         if(missing(range)) { if(impord) {r=1:30} else {r=1:ni}}
         else { r=range}
         if (length(r)>ni) r=r[1:ni]
         if(donames) {
            names=scan(paste(GetRF_WORKING_DIR(),'/varnames',sep=''),what='',quiet=T)
            if (horiz) {
               barplot(vii[r],names=names[o[r]],xlim=c(0,100),las=1,col=col,
                  xlab='Relative importance',horiz=T,cex.names=cex.names)
            }
            else {
               barplot(vii[r],names=names[o[r]],ylim=c(0,100),las=las,col=col,
                  ylab='Relative importance',cex.names=cex.names)
            }
         }
         else {
            barplot(vii[r],ylim=c(0,100),las=las,col=col,
               ylab='Relative importance',xlab='Input variable')
         }
      }
      invisible(list(imp=vii,ord=o))
   }
}
intnull=function(ntimes=10,null.mods=NULL,quiet=F) {
   ntimes <- parchk("ntimes",ntimes,1,1000,10)
   if(!is.logical(quiet)) { quiet=F
      warning('quiet must be logical (T/F). F substituted.')
   }  
   first=missing(null.mods)
   zz=file(paste(GetRF_WORKING_DIR(),'/intparms',sep=''),'rb')
   ip=readBin(zz,integer(),size=8,n=12); close(zz)
   rfstamp=scan(paste(GetRF_WORKING_DIR(),'/rfout',sep=''),what='',quiet=T)
   if(!first) {
      if(!is.list(null.mods)) stop(' input null model must be a list.')
      if(null.mods[[1]][1]!='RuleFit null')
         stop('null.mods is not from Rulefit intnull')
      if(null.mods[[1]][2]!=rfstamp[3] || null.mods[[1]][3]!=rfstamp[4])
        stop('input null.mods inconsistent with RuleFit home directory model.')
      lm=length(null.mods)
      if(!valid.data(null.mods[[lm]],ip[3],ip[4]))
         stop('model inconsistent with input data.')
      ya=null.mods[[lm-2]]
      if(ip[1]==1) {
         if(length(null.mods[[lm-1]])==1)
            stop('regression problem: null.mods = classification model.')
         rs=null.mods[[lm-1]]
      }
      else if(length(null.mods[[lm-1]])!=1) {
         stop('classification problem: null.mods = regression model.')
      }
      check=null.mods[[lm]]
      wd=getwd(); setwd(GetRF_WORKING_DIR())
      if (platform=='windows') {
         system('move /Y rulefit.mod mod.sv',show.output.on.console = F)
         system('move /y rulefit.sum sum.sv',show.output.on.console = F)
         system('move /Y train.y train.sv',show.output.on.console = F)
         system('move /Y rfout out.sv',show.output.on.console = F)
      }
      else {
         system('mv -f rulefit.mod mod.sv')
         system('mv -f rulefit.sum sum.sv')
         system('mv -f train.y train.sv')
         system('mv -f rfout out.sv')
      }
      ita=lm-3
   }
   else {
      iq=ip; iq[6]=-2
      zz=file(paste(GetRF_WORKING_DIR(),'/intparms',sep=''),'wb')
      writeBin(as.integer(iq),zz,size=8); close(zz)
      wd=getwd(); setwd(GetRF_WORKING_DIR())
      if (platform=='windows') {
         system('move /Y rulefit.mod mod.sv',show.output.on.console = F)
         system('move /Y rulefit.sum sum.sv',show.output.on.console = F)
         system('move /Y rfout out.sv',show.output.on.console = F)
      }
      else {
         system('mv -f rulefit.mod mod.sv')
         system('mv -f rulefit.sum sum.sv')
         system('mv -f rfout out.sv')
      }
      status=rfexe('rulefit',minimized=quiet)
      if(status!='OK') { rfstat()
         if (platform=='windows') {
            system('move /Y mod.sv rulefit.mod',show.output.on.console = F)
            system('move /Y sum.sv rulefit.sum',show.output.on.console = F)
            system('move /Y out.sv rfout',show.output.on.console = F)
         }
         else {
            system('mv -f mod.sv rulefit.mod')
            system('mv -f sum.sv rulefit.sum')
            system('mv -f out.sv rfout')
         }
         zz=file(paste(GetRF_WORKING_DIR(),'/intparms',sep=''),'wb')
         writeBin(as.integer(ip),zz,size=8); close(zz)
         setwd(wd); stop()
      }
      ya=train.pred(ip)
      if(ip[1]==2) { ya=1.0/(1.0+exp(-ya))}
      else {
         zz=file(paste(GetRF_WORKING_DIR(),'/train.y',sep=''),'rb')
         y=readBin(zz,numeric(),size=8,n=ip[3]); close(zz)
         rs=y-ya
      }
      zz=file(paste(GetRF_WORKING_DIR(),'/intparms',sep=''),'wb')
      writeBin(as.integer(ip),zz,size=8); close(zz)
      if (platform=='windows') {
         system('move /Y train.y train.sv',show.output.on.console = F)
      }
      else { system('mv -f train.y train.sv')}
      null.mods=list()
      null.mods[[1]]=c('RuleFit null',rfstamp[3],rfstamp[4]);
      ita=1
   }
   for (it in 1:ntimes) { r=runif(ip[3])
      if(ip[1]==1) { o=order(r); yr=ya+rs[o]}
      else { yr=rep(-1.0,ip[3]); yr[ya>=r]=1.0}
      zz=file(paste(GetRF_WORKING_DIR(),'/train.y',sep=''),'wb')
      writeBin(as.double(yr),zz,size=8); close(zz)
      status=rfexe('rulefit',minimized=quiet)
      if(status!='OK') { rfstat()
         if (platform=='windows') {
            system('move /Y mod.sv rulefit.mod',show.output.on.console = F)
            system('move /Y sum.sv rulefit.sum',show.output.on.console = F)
            system('move /Y train.sv train.y',show.output.on.console = F)
            system('move /Y out.sv rfout',show.output.on.console = F)
         }
         else {
            system('mv -f mod.sv rulefit.mod')
            system('mv -f sum.sv rulefit.sum')
            system('mv -f train.sv train.y')
            system('mv -f out.sv rfout')
         }
         setwd(wd); stop()
      }
      zz=file(paste(GetRF_WORKING_DIR(),'/rulefit.mod',sep=''),'rb')
      info=readBin(zz,integer(),size=8,n=6); close(zz)
      len=10+15*info[1]+2*info[2]+2*info[3]+12*info[6]
      zz=file(paste(GetRF_WORKING_DIR(),'/rulefit.mod',sep=''),'rb')
      null.mods[[it+ita]]=readBin(zz,integer(),size=8,n=len); close(zz)
   }
   if (platform=='windows') {
      system('move /Y mod.sv rulefit.mod',show.output.on.console = F)
      system('move /Y sum.sv rulefit.sum',show.output.on.console = F)
      system('move /Y train.sv train.y',show.output.on.console = F)
      system('move /Y out.sv rfout',show.output.on.console = F)
   }
   else {
      system('mv -f mod.sv rulefit.mod')
      system('mv -f sum.sv rulefit.sum')
      system('mv -f train.sv train.y')
      system('mv -f out.sv rfout')
   }
   setwd(wd)
   null.mods[[it+ita+1]]=ya
   if(ip[1]==1) {null.mods[[it+ita+2]]=rs}
   else { null.mods[[it+ita+2]]='class'}
   if(!first) { null.mods[[it+ita+3]]=check}
   else {null.mods[[it+ita+3]]=check.data(ip[3],ip[4])}
   invisible(null.mods)
}
train.pred=function (ip) {
   status=rfexe('rulefit_pred0')
   if(status!='OK') { rfstat();
      if (platform=='windows') {
         system('move /Y mod.sv rulefit.mod',show.output.on.console = F)
         system('move /Y sum.sv rulefit.sum',show.output.on.console = F)
         system('move /Y out.sv rfout',show.output.on.console = F)
      }
      else {
         system('mv -f mod.sv rulefit.mod')
         system('mv -f sum.sv rulefit.sum')
         system('mv -f out.sv rfout')
      }     
      zz=file(paste(GetRF_WORKING_DIR(),'/intparms',sep=''),'wb')
      writeBin(as.integer(ip),zz,size=8); close(zz)
      setwd(wd); stop()
   }
   zz=file(paste(GetRF_WORKING_DIR(),'/yhat',sep=''),'rb')
   info=readBin(zz,numeric(),size=8,n=1); close(zz)
   zz=file(paste(GetRF_WORKING_DIR(),'/yhat',sep=''),'rb')
   yp=readBin(zz,numeric(),size=8,n=info+1); close(zz)
   yp[2:(info+1)]
}
interact=function(vars,null.mods=NULL,nval=100,plot=T,horiz=F,las=2,
   cex.names=1,col=c('red','yellow'),ymax=NULL) {
   zz=file(paste(GetRF_WORKING_DIR(),'/intparms',sep=''),'rb')
   ip=readBin(zz,integer(),size=8,n=4); close(zz)
   nval=min(nval,ip[3])
   nval <- parchk("nval",nval,10,ip[3],100)
   las <- parchk("las",las,1,2,2)
   if(!missing(ymax))  ymax <- parchk("ymax",ymax,0,1,1)
   if(!is.character(col) || length(col)!=2) { col=c('red','yellow')
      warning('col invalid. c("red","yellow") substituted.')
   }
   if(!is.logical(plot)) { plot=T
      warning('plot must be logical (T/F). T substituted.')
   }
   zz=file(paste(GetRF_WORKING_DIR(),'/lx',sep=''),'rb')
   lx=readBin(zz,integer(),size=8,n=ip[4]); close(zz)
   varnames=scan(paste(GetRF_WORKING_DIR(),'/varnames',sep=''),what='',quiet=T)
   iv=getvars(vars,ip[4],lx,varnames)
   iv=iv[iv!=0]; nvar=length(iv)
   if(nvar==0) stop(' no valid input variables.')
   ii=c(nvar,iv,nval)
   zz=file(paste(GetRF_WORKING_DIR(),'/interactparms',sep=''),'wb')
   writeBin(as.integer(ii),zz,size=8); close(zz)
   wd=getwd(); setwd(GetRF_WORKING_DIR())
   status=rfexe('interact')
   if(status!='OK') { rfstat(); setwd(wd); stop()}
   zz=file(paste(GetRF_WORKING_DIR(),'/interact',sep=''),'rb')
   int=readBin(zz,numeric(),size=8,n=nvar); close(zz)
   if (missing(null.mods)) {
      if(plot) { if(missing(ymax)) ymax=1.1*max(int)
         if (horiz) {
            barplot(int,names=varnames[iv],las=1,xlim=c(0,ymax),
               col=col[2],xlab='Interaction strength',
               horiz=T,cex.names=cex.names)
         }
         else {
            barplot(int,names=varnames[iv],las=las,ylim=c(0,ymax),
            col=col[2],ylab='Interaction strength',cex.names=cex.names)
         }
      }
      setwd(wd); invisible(int)
   }
   else {
      if(!is.list(null.mods)) { setwd(wd)
         stop(' input null model must be a list.')
      }
      if(null.mods[[1]][1]!='RuleFit null') { setwd(wd)
         stop('null.mods is not from Rulefit intnull')
      }
      rfstamp=scan(paste(GetRF_WORKING_DIR(),'/rfout',sep=''),what='',quiet=T)
      if(null.mods[[1]][2]!=rfstamp[3] || null.mods[[1]][3]!=rfstamp[4]) {
         setwd(wd)
        stop('input null.mods inconsistent with RuleFit home directory model.')
      } 
      lm=length(null.mods)
      if(!valid.data(null.mods[[lm]],ip[3],ip[4])) { setwd(wd)
         stop('model inconsistent with input data.')
      }
      if(ip[1]==1) {
         if(length(null.mods[[lm-1]])==1) { setwd(wd)
            stop('regression problem: null.mods = classification model.')
         }
      }
      else if(length(null.mods[[lm-1]])!=1) { setwd(wd)
         stop('classification problem: null.mods = regression model.')
      }
      if (platform=='windows') {
         system('move /Y rulefit.mod mod.sv',show.output.on.console = F)
      }
      else { system('mv -f rulefit.mod mod.sv')}
      times=lm-4;
      repint=matrix(nrow=nvar,ncol=times)
      for (it in 1:times) {
         zz=file(paste(GetRF_WORKING_DIR(),'/rulefit.mod',sep=''),'wb')
         writeBin(as.integer(null.mods[[it+1]]),zz,size=8); close(zz)
         status=rfexe('interact')
         if(status!='OK') { rfstat()
            if (platform=='windows') {
               system('move /Y mod.sv rulefit.mod',show.output.on.console = F)
            }
            else { system('mv -f mod.sv rulefit.mod')}
            setwd(wd); stop()
         }
         zz=file(paste(GetRF_WORKING_DIR(),'/interact',sep=''),'rb')
         repint[,it]=readBin(zz,numeric(),size=8,n=nvar); close(zz)
      }
      if (platform=='windows') {
         system('move /Y mod.sv rulefit.mod',show.output.on.console = F)
      }
      else { system('mv -f mod.sv rulefit.mod')}
      setwd(wd)
      nullave=rep(0,nvar); nullstd=rep(0,nvar);
      for (i in 1:nvar) {
         nullave[i]=mean(repint[i,]); nullstd[i]=sqrt(var(repint[i,]))
      }
      if(plot) { if(missing(ymax)) ymax=1.1*max(int-nullave,nullstd)
         v=matrix(nrow=2,ncol=length(int))
         v[1,]=nullstd; v[2,]=pmax(int-nullave-nullstd,0)
         if (horiz) {         
            barplot(v,names=varnames[iv],las=1,xlim=c(0,ymax),
               col=col,horiz=T,
               cex.names=cex.names,xlab='Null adjusted interaction strength')
         }
         else {
            barplot(v,names=varnames[iv],las=las,ylim=c(0,ymax),
               col=col,cex.names=cex.names,
               ylab='Null adjusted interaction strength')
         }
      }
      invisible(list(int=int,nullave=nullave,nullstd=nullstd))
   }
}
rfexe=function(program,minimized=T) {
   write(program,file=paste(GetRF_WORKING_DIR(),'/program',sep=''))
   if (platform=='windows') {
      if(minimized) { system(file.path(GetRF_HOME(),'rf_go.exe'),show.output.on.console=F,wait=T)}
      else {
         system(paste('cmd /c', file.path(GetRF_HOME(), 'rf_go.exe')),invisible=F,
            show.output.on.console=F,wait=T,minimized=F)
       }
   }
   else {
      if(minimized) { system(paste(file.path(GetRF_HOME(), 'rf_go.exe'), '> temp.out'))}
      else { system(file.path(GetRF_HOME(),'rf_go.exe'))}
   }
   status=scan(paste(GetRF_WORKING_DIR(),'/rfstatus',sep=''),what='',quiet=T)
   status[1]
}
twovarint=function(tvar,vars,null.mods=NULL,nval=100,import=F,plot=T,
   horiz=F,las=2,cex.names=1,col=c('red','yellow'),ymax=NULL) {
   zz=file(paste(GetRF_WORKING_DIR(),'/intparms',sep=''),'rb')
   ip=readBin(zz,integer(),size=8,n=4); close(zz)
   nval=min(nval,ip[3])
   nval <- parchk("nval",nval,10,ip[3],100)
   las <- parchk("las",las,1,2,2)
   if(!missing(ymax))  ymax <- parchk("ymax",ymax,0,1,1)
   if(!is.character(col) || length(col)!=2) { col=c('red','yellow')
      warning('col invalid. c("red","yellow") substituted.')
   }
   if(!is.logical(plot)) { plot=T
      warning('plot must be logical (T/F). T substituted.')
   }
   if(!is.logical(import)) { import=F
      warning('import must be logical (T/F). F substituted.')
   }
   zz=file(paste(GetRF_WORKING_DIR(),'/lx',sep=''),'rb')
   lx=readBin(zz,integer(),size=8,n=ip[4]); close(zz)
   varnames=scan(paste(GetRF_WORKING_DIR(),'/varnames',sep=''),what='',quiet=T)
   iv=getvars(tvar,ip[4],lx,varnames)
   if(iv[1]==0) stop(' target variable invalid.')
   itvar=iv[1]    
   iv=getvars(vars,ip[4],lx,varnames)
   iv=iv[iv!=0]; nvar=length(iv)
   if(nvar==0) stop(' no valid input variables.')
   k=(1:nvar)[iv==itvar]
   if(length(k)!=0) stop('target variable same as a input variable.')
   if(!is.logical(import)) stop(' import must be TRUE or FALSE.')
   ii=c(nvar,itvar,iv,nval,as.integer(import))
   zz=file(paste(GetRF_WORKING_DIR(),'/twovarparms',sep=''),'wb')
   writeBin(as.integer(ii),zz,size=8); close(zz)
   wd=getwd(); setwd(GetRF_WORKING_DIR())
   status=rfexe('twovar')
   if(status!='OK') { rfstat(); setwd(wd); stop()}
   zz=file(paste(GetRF_WORKING_DIR(),'/twovar',sep=''),'rb')
   int=readBin(zz,numeric(),size=8,n=nvar); close(zz)
   if (missing(null.mods)) {
      if(plot) { if(missing(ymax)) ymax=1.1*max(int)
         if (horiz) {
            barplot(int,names=varnames[iv],las=1,xlim=c(0,ymax),
               col=col[2],horiz=T,
               cex.names=cex.names,
               xlab=paste('Interaction strength with ',varnames[itvar]))
         }
         else {
           barplot(int,names=varnames[iv],las=las,ylim=c(0,ymax),
               col=col[2],cex.names=cex.names,
               ylab=paste('Interaction strength with ',varnames[itvar]))
         }
      }
      setwd(wd); invisible(int)
   }
   else {
      if(!is.list(null.mods)) { setwd(wd)
         stop(' input null model must be a list.')
      }
      if(null.mods[[1]][1]!='RuleFit null') { setwd(wd)
         stop('null.mods is not from Rulefit intnull')
      }
      rfstamp=scan(paste(GetRF_WORKING_DIR(),'/rfout',sep=''),what='',quiet=T)
      if(null.mods[[1]][2]!=rfstamp[3] || null.mods[[1]][3]!=rfstamp[4]) {
         setwd(wd)
        stop('input null.mods inconsistent with RuleFit home directory model.')
      }
      lm=length(null.mods)
      if(!valid.data(null.mods[[lm]],ip[3],ip[4])) { setwd(wd)
         stop('model inconsistent with input data.')
      }
      if(ip[1]==1) {
         if(length(null.mods[[lm-1]])==1) { setwd(wd)
            stop('regression problem: null.mods = classification model.')
         }
      }
      else if(length(null.mods[[lm-1]])!=1) { setwd(wd)
         stop('classification problem: null.mods = regression model.')
      }
      if (platform=='windows') {
         system('move /Y rulefit.mod mod.sv',show.output.on.console = F)
      }
      else { system('mv -f rulefit.mod mod.sv')}
      times=lm-4;
      repint=matrix(nrow=nvar,ncol=times)
      for (it in 1:times) {
         zz=file(paste(GetRF_WORKING_DIR(),'/rulefit.mod',sep=''),'wb')
         writeBin(as.integer(null.mods[[it+1]]),zz,size=8); close(zz)
         status=rfexe('twovar')
         if(status!='OK') { rfstat()
            if (platform=='windows') {
               system('move /Y mod.sv rulefit.mod',show.output.on.console = F)
            }
            else { system('mv -f mod.sv rulefit.mod')}
            setwd(wd); stop()
         }
         zz=file(paste(GetRF_WORKING_DIR(),'/twovar',sep=''),'rb')
         repint[,it]=readBin(zz,numeric(),size=8,n=nvar); close(zz)
      }
      if (platform=='windows') {
         system('move /Y mod.sv rulefit.mod',show.output.on.console = F)
      }
      else { system('mv -f mod.sv rulefit.mod')}
      setwd(wd)
      nullave=rep(0,nvar); nullstd=rep(0,nvar);
      for (i in 1:nvar) {
         nullave[i]=mean(repint[i,]); nullstd[i]=sqrt(var(repint[i,]))
      }
      if(plot) { if(missing(ymax)) ymax=1.1*max(int-nullave,nullstd)
         v=matrix(nrow=2,ncol=length(int))
         v[1,]=nullstd; v[2,]=pmax(int-nullave-nullstd,0)
         if (horiz) {         
            barplot(v,names=varnames[iv],las=1,xlim=c(0,ymax),
               col=col,xlab=paste
               ('Null adjusted interaction strength with ',varnames[itvar]),
               horiz=T,cex.names=cex.names)
         }
         else {
           barplot(v,names=varnames[iv],las=las,ylim=c(0,ymax),
               col=col,ylab=paste
               ('Null adjusted interaction strength with ',varnames[itvar]),
               cex.names=cex.names)
         }
      }
      invisible(list(int=int,nullave=nullave,nullstd=nullstd))
   }
 }
threevarint=function(tvar1,tvar2,vars,null.mods=NULL,nval=100,import=F,
   plot=T,horiz=F,las=2,cex.names=1,col=c('red','yellow'),ymax=NULL) {
   zz=file(paste(GetRF_WORKING_DIR(),'/intparms',sep=''),'rb')
   ip=readBin(zz,integer(),size=8,n=4); close(zz)
   nval=min(nval,ip[3])
   nval <- parchk("nval",nval,10,ip[3],100)
   las <- parchk("las",las,1,2,2)
   if(!missing(ymax))  ymax <- parchk("ymax",ymax,0,1,1)
   if(!is.character(col) || length(col)!=2) { col=c('red','yellow')
      warning('col invalid. c("red","yellow") substituted.')
   }
   if(!is.logical(plot)) { plot=T
      warning('plot must be logical (T/F). T substituted.')
   }
   if(!is.logical(import)) { import=F
      warning('import must be logical (T/F). F substituted.')
   }
   zz=file(paste(GetRF_WORKING_DIR(),'/lx',sep=''),'rb')
   lx=readBin(zz,integer(),size=8,n=ip[4]); close(zz)
   varnames=scan(paste(GetRF_WORKING_DIR(),'/varnames',sep=''),what='',quiet=T)
   iv=getvars(c(tvar1,tvar2),ip[4],lx,varnames)
   itvar=iv[iv!=0]; nvar=length(itvar)
   if(nvar!=2) stop(' target variables invalid.') 
   iv=getvars(vars,ip[4],lx,varnames)
   iv=iv[iv!=0]; nvar=length(iv)
   if(nvar==0) stop(' no valid input variables.')
   k1=(1:nvar)[iv==itvar[1]]; k2=(1:nvar)[iv==itvar[2]]
   if((length(k1)+length(k2))!=0)
      stop('target variable same as an input variable.') 
   if(!is.logical(import)) stop(' import must be TRUE or FALSE.')
   ii=c(nvar,itvar,iv,nval,as.integer(import))
   zz=file(paste(GetRF_WORKING_DIR(),'/threevarparms',sep=''),'wb')
   writeBin(as.integer(ii),zz,size=8); close(zz)
   wd=getwd(); setwd(GetRF_WORKING_DIR())
   status=rfexe('threevar')
   if(status!='OK') { rfstat(); setwd(wd); stop()}
   zz=file(paste(GetRF_WORKING_DIR(),'/threevar',sep=''),'rb')
   int=readBin(zz,numeric(),size=8,n=nvar); close(zz)
   if (missing(null.mods)) {
      if(plot) { if(missing(ymax)) ymax=1.1*max(int)
         if (horiz) {
            barplot(int,names=varnames[iv],las=1,xlim=c(0,ymax),
               col=col[2],xlab=paste('Interaction strength with ',
               varnames[itvar[1]],' & ',varnames[itvar[2]]),
               horiz=T,cex.names=cex.names)
         }
         else {
            barplot(int,names=varnames[iv],las=las,ylim=c(0,ymax),
               col=col[2],ylab=paste('Interaction strength with ',
               varnames[itvar[1]],' & ',varnames[itvar[2]]),
               cex.names=cex.names)
         }
      }
      setwd(wd); invisible(int)
   }
   else {
      if(!is.list(null.mods)) { setwd(wd)
         stop(' input null model must be a list.')
      }
      if(null.mods[[1]][1]!='RuleFit null') { setwd(wd)
         stop('null.mods is not from Rulefit intnull')
      }
      rfstamp=scan(paste(GetRF_WORKING_DIR(),'/rfout',sep=''),what='',quiet=T)
      if(null.mods[[1]][2]!=rfstamp[3] || null.mods[[1]][3]!=rfstamp[4]) {
         setwd(wd)
         stop(' null.mods inconsistent with RuleFit home directory model.')
      }
      lm=length(null.mods)
      if(!valid.data(null.mods[[lm]],ip[3],ip[4])) { setwd(wd)
         stop('model inconsistent with input data.')
      }
      if(ip[1]==1) {
         if(length(null.mods[[lm-1]])==1) { setwd(wd)
            stop('regression problem: null.mods = classification model.')
         }
      }
      else if(length(null.mods[[lm-1]])!=1) { setwd(wd)
         stop('classification problem: null.mods = regression model.')
      }
      if (platform=='windows') {
         system('move /Y rulefit.mod mod.sv',show.output.on.console = F)
      }
      else { system('mv -f rulefit.mod mod.sv')}
      times=lm-4;
      repint=matrix(nrow=nvar,ncol=times)
      for (it in 1:times) {
         zz=file(paste(GetRF_WORKING_DIR(),'/rulefit.mod',sep=''),'wb')
         writeBin(as.integer(null.mods[[it+1]]),zz,size=8); close(zz)
         status=rfexe('threevar')
         if(status!='OK') { rfstat();
            if (platform=='windows') {
               system('move /Y mod.sv rulefit.mod',show.output.on.console = F)
            }
            else { system('mv -f mod.sv rulefit.mod')}
            setwd(wd); stop()
         }
         zz=file(paste(GetRF_WORKING_DIR(),'/threevar',sep=''),'rb')
         repint[,it]=readBin(zz,numeric(),size=8,n=nvar); close(zz)
      }
      if (platform=='windows') {
         system('move /Y mod.sv rulefit.mod',show.output.on.console = F)
      }
      else { system('mv -f mod.sv rulefit.mod')}
      setwd(wd)
      nullave=rep(0,nvar); nullstd=rep(0,nvar);
      for (i in 1:nvar) {
         nullave[i]=mean(repint[i,]); nullstd[i]=sqrt(var(repint[i,]))
      }
      if(plot) { if(missing(ymax)) ymax=1.1*max(int-nullave,nullstd)
         v=matrix(nrow=2,ncol=length(int))
         v[1,]=nullstd; v[2,]=pmax(int-nullave-nullstd,0)
         if (horiz) {
            barplot(v,names=varnames[iv],las=1,xlim=c(0,ymax),
               col=col,xlab=paste('Null adjusted interaction strength with ',
               varnames[itvar[1]],' & ',varnames[itvar[2]]),
               horiz=T,cex.names=cex.names)
         }
         else {
            barplot(v,names=varnames[iv],las=las,ylim=c(0,ymax),
               col=col,ylab=paste('Null adjusted interaction strength with ',
               varnames[itvar[1]],' & ',varnames[itvar[2]]),
               cex.names=cex.names)
         }
      }
      invisible(list(int=int,nullave=nullave,nullstd=nullstd))
   }
}
check.data=function(n,p) {
   nt=min(n,100)
   zz=file(paste(GetRF_WORKING_DIR(),'/train.x',sep=''),'rb')
   t1=readBin(zz,numeric(),size=8,n=nt); close(zz)
   zz=file(paste(GetRF_WORKING_DIR(),'/train.y',sep=''),'rb')
   t2=readBin(zz,numeric(),size=8,n=nt); close(zz)
   zz=file(paste(GetRF_WORKING_DIR(),'/train.w',sep=''),'rb')
   t3=readBin(zz,numeric(),size=8,n=nt); close(zz)
   c(nt,n,p,t1,t2,t3)
}
valid.data=function(model,n,p) {
   nt=min(n,100); u=check.data(n,p)
   if(u[1]!=nt) { F}
   else if (any(u!=model)) { F}
   else { T}
}
rules=function(beg=1,end=beg+9,x=NULL,wt=rep(1,n)) {
   beg <- parchk("beg",beg,1,1000000,1)
   end <- parchk("end",end,beg,1000000,beg+9)
   zz=file(paste(GetRF_WORKING_DIR(),'/intparms',sep=''),'rb')
   ip=readBin(zz,integer(),size=8,n=4); close(zz); ni=ip[4]
   if(!missing(x)) { mode=1
      if(is.data.frame(x)) { p=length(x); n=length(x[[1]])
         xx=matrix(nrow=n,ncol=p); for (j in 1:p) { xx[,j]=x[[j]]}
      }
      else if(is.matrix(x)) { n <- nrow(x); p <- ncol(x); xx=x;}
      else if(is.vector(x)) { p=length(x); n=1; xx=x}
      else { stop(' x must be a data frame, matrix, or vector.')}
      if(p!=ni)
         stop(paste(' number of variables =',as.character(p),
            'is different from training data =',as.character(ni)))
      if(any(is.na(xx))) {
         zz=file(paste(GetRF_WORKING_DIR(),'/realparms',sep=''),'rb')
         xmiss=readBin(zz,numeric(),size=8,n=1); close(zz)
         xx[is.na(xx)] <- xmiss;
      }
      zz=file(paste(GetRF_WORKING_DIR(),'/varimp.x',sep=''),'wb')
      writeBin(as.numeric(c(n,as.vector(xx))),zz,size=8); close(zz)
      putw(wt,xx,n,p,'varimp.w')
   }
   else { mode=0}
   zz=file(paste(GetRF_WORKING_DIR(),'/intrules',sep=''),'wb')
   writeBin(as.integer(c(mode,beg,end)),zz,size=8); close(zz)
   wd=getwd(); setwd(GetRF_WORKING_DIR())
   status=rfexe('rules')
   setwd(wd)
   if(status!='OK') { rfstat(); stop()}
   rfhelp('rulesout')
   invisible()
}
rfannotate=function(model,text) {
   if(!is.list(model)) stop(' input model must be a list.')
   if(model[[1]][1]!='RuleFit') stop('model not from RuleFit')
   thecall=as.character(text); model[[9]]=thecall
   save(thecall,file=paste(GetRF_WORKING_DIR(),'/thecall',sep=''))
   invisible(model)
}
rfmodinfo=function(model) {
   if(!is.list(model)) stop(' input model must be a list.')
   if(model[[1]][1]!='RuleFit') stop('model not from RuleFit')
   if(length(model)>8) print(model[[9]],quote=F)
   writerfout(model[[1]],2)
   cat('Parameters:\n')
   u=paste(model[[6]][model[[7]]==2],collapse=' ')
   cat(paste('   cat.vars =',u,'\n'))
   u=paste(model[[6]][model[[7]]==0],collapse=' ')
   cat(paste('   not.used =',u,'\n'))
   cat(paste('   xmiss =',signif(model[[5]][1],7),'\n'))
   if(model[[4]][1]==1) { text='regress'} else { text='class'}
   cat(paste('   rfmode =',text,'\n'))
   cat(paste('   sparse =',model[[5]][10],'\n'))
   cat(paste('   test.reps =',model[[4]][7],'\n'))
   cat(paste('   test.fract =',signif(model[[5]][3],7),'\n'))
   cat(paste('   mod.sel =',model[[4]][8],'\n'))
   if(model[[4]][2]==0) { text='linear'}
   else if(model[[4]][2]==1){ text='rules'}
   else {text='both'}
   cat(paste('   model.type =',text,'\n'))
   cat(paste('   tree.size =',model[[4]][6],'\n'))
   cat(paste('   max.rules =',model[[4]][5],'\n'))
   cat(paste('   max.trms =',model[[4]][9],'\n'))
   if (model[[4]][1] != 1) {
      cat(paste('   costs[1] =',signif(model[[5]][7],7),'\n'))
      cat(paste('   costs[2] =',signif(model[[5]][8],7),'\n'))
   }
   cat(paste('   trim.qntl =',signif(model[[5]][2],7),'\n'))
   cat(paste('   samp.fract =',signif(model[[5]][6],7),'\n'))
   cat(paste('   inter.supp =',signif(model[[5]][4],7),'\n'))
   cat(paste('   memory.par =',signif(model[[5]][5],7),'\n'))
   cat(paste('   conv.thr =',signif(model[[5]][9],7),'\n'))
   invisible()
}
rfnullinfo=function(model) {
   if(!is.list(model)) stop(' input null model must be a list.')
   if(model[[1]][1]!='RuleFit null')
      stop('input model not a RuleFit null model object')
   cat(paste(model[[1]][1],'model object.\n'))
   cat(
   paste('Contains',length(model)-4,'bootstrapped null interaction models\n'))
   cat(paste('to be used with RuleFit model created on',model[[1]][2],
      model[[1]][3],'\n'))
   invisible()
}
rfxval=function(nfold=10,quiet=F) {
   zz=file(paste(GetRF_WORKING_DIR(),'/intparms',sep=''),'rb')
   ip=readBin(zz,integer(),size=8,n=12); close(zz)
   nfold <- parchk("nfold",nfold,2,ip[3],10)
   if(!is.logical(quiet)) { quiet=F
      warning('quiet must be logical (T/F). F substituted.')
   }
   zz=file(paste(GetRF_WORKING_DIR(),'/train.y',sep=''),'rb')
   y=readBin(zz,numeric(),size=8,n=ip[3]); close(zz)
   zz=file(paste(GetRF_WORKING_DIR(),'/train.w',sep=''),'rb')
   wt=readBin(zz,numeric(),size=8,n=ip[3]); close(zz)
   zz=file(paste(GetRF_WORKING_DIR(),'/train.x',sep=''),'rb')
   xx=readBin(zz,numeric(),size=8,n=ip[3]*ip[4]); close(zz)
   xx=matrix(xx,nrow=ip[3],ncol=ip[4])
   o=order(y); y=y[o]; wt=wt[o]; xx=xx[o,]
   wd=getwd(); setwd(GetRF_WORKING_DIR())
   if (platform=='windows') {
      system('move /Y rulefit.mod mod.sv',show.output.on.console = F)
      system('move /Y rulefit.sum sum.sv',show.output.on.console = F)
      system('move /Y train.y y.sv',show.output.on.console = F)
      system('move /Y train.w w.sv',show.output.on.console = F)
      system('move /Y train.x x.sv',show.output.on.console = F)
      system('move /Y rfout out.sv',show.output.on.console = F)
      system('move /Y intparms parm.sv',show.output.on.console = F)
   }
   else {
      system('mv -f rulefit.mod mod.sv')
      system('mv -f rulefit.sum sum.sv')
      system('mv -f train.y y.sv')
      system('mv -f train.w w.sv')
      system('mv -f train.x x.sv')
      system('mv -f rfout out.sv')
      system('mv -f intparms parm.sv')
   }
   im=0; ml=rep(0,ip[3]); mt=rep(0,ip[3]); yx=rep(0,ip[3]); iq=ip
   for (it in 1:nfold) { nl=0; nt=0
      for (i in 1:ip[3]) {
         if((i+im)%%nfold!=0) { nl=nl+1; ml[nl]=i}
         else { nt=nt+1; mt[nt]=i}
      }
      if (max(y[ml[1:nl]])==min(y[ml[1:nl]])) stop('all y values in subsample are the same.')
      zz=file(paste(GetRF_WORKING_DIR(),'/train.y',sep=''),'wb')
      writeBin(as.double(y[ml[1:nl]]),zz,size=8); close(zz)
      zz=file(paste(GetRF_WORKING_DIR(),'/train.w',sep=''),'wb')
      writeBin(as.double(wt[ml[1:nl]]),zz,size=8); close(zz)
      zz=file(paste(GetRF_WORKING_DIR(),'/train.x',sep=''),'wb')
      writeBin(as.double(as.vector(xx[ml[1:nl],])),zz,size=8); close(zz)
      iq[3]=nl
      zz=file(paste(GetRF_WORKING_DIR(),'/intparms',sep=''),'wb')
      writeBin(as.integer(iq),zz,size=8); close(zz)
      status=rfexe('rulefit',minimized=quiet)
      if(status!='OK') { rfstat()
         if (platform=='windows') {
            system('move /Y mod.sv rulefit.mod',show.output.on.console = F)
            system('move /Y sum.sv rulefit.sum',show.output.on.console = F)
            system('move /Y y.sv train.y',show.output.on.console = F)
            system('move /Y w.sv train.w',show.output.on.console = F)
            system('move /Y x.sv train.x',show.output.on.console = F)
            system('move /Y out.sv rfout',show.output.on.console = F)
            system('move /Y parm.sv intparms',show.output.on.console = F)
         }
         else {
            system('mv -f mod.sv rulefit.mod')
            system('mv -f sum.sv rulefit.sum')
            system('mv -f y.sv train.y')
            system('mv -f w.sv train.w')
            system('mv -f x.sv train.x')
            system('mv -f out.sv rfout')
            system('mv -f parm.sv intparms')
         }
         setwd(wd); stop()
      }
      yx[mt[1:nt]]=rfpred(xx[mt[1:nt],])
      im=im+1
   }
   zz=file(paste(GetRF_WORKING_DIR(),'/intparms',sep=''),'wb')
   writeBin(as.integer(ip),zz,size=8); close(zz)
   if (platform=='windows') {
      system('move /Y mod.sv rulefit.mod',show.output.on.console = F)
      system('move /Y sum.sv rulefit.sum',show.output.on.console = F)
      system('move /Y y.sv train.y',show.output.on.console = F)
      system('move /Y w.sv train.w',show.output.on.console = F)
      system('move /Y x.sv train.x',show.output.on.console = F)
      system('move /Y out.sv rfout',show.output.on.console = F)
      system('move /Y parm.sv intparms',show.output.on.console = F)
   }
   else {
      system('mv -f mod.sv rulefit.mod')
      system('mv -f sum.sv rulefit.sum')
      system('mv -f y.sv train.y')
      system('mv -f w.sv train.w')
      system('mv -f x.sv train.x')
      system('mv -f out.sv rfout')
      system('mv -f parm.sv intparms')
   }
   setwd(wd)
   cat(' cross-validated:\n')
   if(ip[1]==1) { sw=sum(wt)
      aae=sum(wt*abs(y-yx))/sw; rmse=sqrt(sum(wt*(y-yx)**2)/sw)
      cat(paste('   ave abs error =',signif(aae,4),'\n'))
      cat(paste('   rms error =',signif(rmse,4),'\n'))
      yx[o]=yx
      invisible(list(yp=yx,aae=aae,rmse=rmse))
   }
   else { err=rep(0,ip[3]); err[which(y*yx<0)]=1; auc=1-aroc(y,wt,yx)
      errave=sum(wt[which(err==1)])/sum(wt)
      errpos=sum(wt[which(err==1 & y>0)])/sum(wt[which(y>0)])
      errneg=sum(wt[which(err==1 & y<0)])/sum(wt[which(y<0)])
      cat(paste('   1-AUC =',signif(auc,4),'\n'))
      cat(paste('   average  error rate =',signif(errave,4),'\n'))
      cat(paste('   positive error rate =',signif(errpos,4),'\n'))
      cat(paste('   negative error rate =',signif(errneg,4),'\n'))
      yx[o]=yx
      invisible(list(lo=yx,omAUC=auc,errave=errave,errpos=errpos,errneg=errneg))
   }
}
aroc=function(y,w,s) {
   o=order(s); w01=0; wroc=0
   for(i in 1:length(y)) {
      if(y[o[i]]>0) { wroc=wroc+w[o[i]]*w01}
      else { w01=w01+w[o[i]]}
   }
   wroc/(w01*(sum(w)-w01))
}
runstats=function(rfmod) {
   if(missing(rfmod)) {
      it=scan(paste(GetRF_WORKING_DIR(),'/rfout',sep=''),what='',quiet=T)
   }
   else { it=rfmod[[1]]}
   list(cri=as.numeric(it[11]),err=as.numeric(it[12]),terms=as.numeric(it[13]))
}
getintercept=function() {
  # Returns the intercept of the RuleFit model in the working directory.
  zz <- file(file.path(GetRF_WORKING_DIR(), 'rulefit.mod'), 'rb')
  c0 <- readBin(zz, numeric(), size=8, n=8)[8]
  close(zz)
  return(c0)
}

