!
!     CalculiX - A 3-dimensional finite element program
!              Copyright (C) 1998-2018 Guido Dhondt
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation(version 2);
!     
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of 
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program; if not, write to the Free Software
!     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
      subroutine calinput(co,nk,kon,ipkon,lakon,nkon,
     &  ne,nodeboun,ndirboun,xboun,nboun,
     &  ipompc,nodempc,coefmpc,nmpc,nmpc_,nodeforc,ndirforc,xforc,nforc,
     &  nforc_,nelemload,sideload,xload,nload,nload_,
     &  nprint,prlab,prset,mpcfree,nboun_,
     &  mei,set,istartset,iendset,ialset,nset,nalset,elcon,nelcon,rhcon,
     &  nrhcon,alcon,nalcon,alzero,t0,t1,
     &  matname,ielmat,orname,orab,ielorien,amname,amta,namta,nam,
     &  nmethod,iamforc,iamload,iamt1,
     &  ithermal,iperturb,istat,istep,nmat,ntmat_,norien,
     &  prestr,iprestr,isolver,fei,veold,timepar,
     &  xmodal,filab,jout,nlabel,idrct,jmax,
     &  iexpl,alpha,iamboun,plicon,nplicon,plkcon,
     &  nplkcon,iplas,npmat_,mi,nk_,trab,inotr,ntrans,ikboun,
     &  ilboun,ikmpc,ilmpc,ics,dcs,ncs_,namtot_,cs,nstate_,ncmat_,iumat,
     &  mcs,labmpc,iponor,xnor,knor,thickn,thicke,ikforc,ilforc,
     &  offset,iponoel,inoel,rig,infree,nshcon,shcon,cocon,ncocon,
     &  physcon,nflow,ctrl,maxlenmpc,ne1d,
     &  ne2d,nener,vold,nodebounold,ndirbounold,xbounold,
     &  xforcold,xloadold,t1old,eme,sti,ener,xstate,jobnamec,
     &  irstrt,ttime,qaold,output,typeboun,inpc,
     &  ipoinp,inp,tieset,tietol,ntie,fmpc,cbody,ibody,xbody,
     &  nbody,nbody_,xbodyold,nam_,ielprop,nprop,nprop_,prop,itpamp,
     &  iviewfile,ipoinpc,nslavs,t0g,t1g,network,cyclicsymmetry,
     &  idefforc,idefload,idefbody,mortar,ifacecount,islavsurf,
     &  pslavsurf,clearini,heading,iaxial,nobject,objectset,nprint_,
     &  iuel,nuel_,nodempcref,coefmpcref,ikmpcref,memmpcref_,
     &  mpcfreeref,maxlenmpcref,memmpc_,isens,namtot,nstam,dacon,
     &  vel,nef,velo,veloo)
!
      implicit none
!
      character*1 typeboun(*),inpc(*)
      character*3 output
      character*6 prlab(*)
      character*8 lakon(*)
      character*20 labmpc(*),sideload(*)
      character*66 heading(*)
      character*80 matname(*),orname(*),amname(*)
      character*81 set(*),prset(*),tieset(3,*),cbody(*),objectset(4,*)
      character*87 filab(*)
      character*132 jobnamec(*),textpart(16)
!
      integer kon(*),nodeboun(*),ndirboun(*),ipompc(*),nodempc(3,*),
     &  nodeforc(2,*),ndirforc(*),nelemload(2,*),iaxial,j,mi(*),
     &  istartset(*),iendset(*),ialset(*),ipkon(*),ics(*),nodedep,
     &  nelcon(2,*),nrhcon(*),nalcon(2,*),ielmat(mi(3),*),nodeind,
     &  ielorien(mi(3),*),icomposite,nsubmodel,mortar,
     &  namta(3,*),iamforc(*),iamload(2,*),iamt1(*),ipoinpc(0:*),
     &  iamboun(*),inotr(2,*),ikboun(*),ilboun(*),ikmpc(*),ilmpc(*),
     &  iponor(2,*),knor(*),ikforc(*),ilforc(*),iponoel(*),inoel(3,*),
     &  infree(4),ixfree,ikfree,inoelfree,iponoelmax,rig(*),nshcon(*),
     &  ncocon(2,*),nodebounold(*),ielprop(*),nprop,nprop_,maxsectors,
     &  ndirbounold(*),ipoinp(2,*),inp(3,*),nintpoint,ifacecount,
     &  ianisoplas,ifile_output,ichangefriction,nslavs,
     &  nalset,nalset_,nmat,nmat_,ntmat_,norien,norien_,islavsurf(2,*),
     &  nmethod,nk,ne,nboun,nmpc,nmpc_,mpcfree,i,istat,n,
     &  key,nk_,ne_,nboun_,ncs_,namtot_,nstate_,iviewfile,
     &  isolver,ithermal(2),iperturb(*),iprestr,istep,mei(4),nkon,
     &  nprint,nload,nload_,nforc,nforc_,nlabel,iumat,imat,
     &  nset,nset_,nprint_,nam,nam_,jout(2),ncmat_,itpamp,
     &  ierror,idrct,jmax(2),iexpl,iplas,npmat_,ntrans,ntrans_,
     &  M_or_SPC,nplicon(0:ntmat_,*),nplkcon(0:ntmat_,*),nflow,
     &  ne1d,ne2d,nener,irstrt(*),ii,maxlenmpc,inl,ipol,network,
     &  iline,mcs,ntie,ntie_,lprev,newstep,nbody,nbody_,ibody(3,*),
     &  cyclicsymmetry,idefforc(*),idefload(*),idefbody(*),
     &  ichangesurfacebehavior,nobject,ibasemotion,iuel(4,*),nuel_,
     &  nodempcref(3,*),ikmpcref(*),memmpcref_,mpcfreeref,
     &  maxlenmpcref,memmpc_,isens,iamplitudedefault,namtot,
     &  nstam,ier,nef
!
      real*8 co(3,*),xboun(*),coefmpc(*),xforc(*),fmpc(*),
     &  xload(2,*),alzero(*),offset(2,*),prop(*),pslavsurf(3,*),
     &  elcon(0:ncmat_,ntmat_,*),rhcon(0:1,ntmat_,*),clearini(3,9,*),
     &  alcon(0:6,ntmat_,*),thicke(mi(3),*),thickn(2,*),xnor(*),
     &  t1(*),orab(7,*),prestr(6,mi(1),*),amta(2,*),dacon(*),
     &  veold(0:mi(2),*),t0(*),plicon(0:2*npmat_,ntmat_,*),
     &  plkcon(0:2*npmat_,ntmat_,*),trab(7,*),dcs(*),
     &  shcon(0:3,ntmat_,*),cocon(0:6,ntmat_,*),timepar(*),
     &  ctrl(*),vold(0:mi(2),*),xbounold(*),xforcold(*),
     &  xloadold(*),t1old(*),eme(*),sti(*),ener(*),
     &  xstate(nstate_,mi(1),*),ttime,qaold(2),cs(17,*),tietol(2,*),
     &  xbody(7,*),xbodyold(7,*),t0g(2,*),t1g(2,*),
     &  fei(3),tinc,tper,xmodal(*),tmin,tmax,tincf,
     &  alpha,physcon(*),coefmpcref(*),vel(nef,*),velo(*),veloo(*)
! JOB FINISHED
      if(istat.lt.0) then
         write(*,*)
         write(*,*) 'Job finished'
         write(*,*)
         return
      endif
! node data 
      co(1,1)=0
      co(2,1)=0
      co(3,1)=0
      co(1,2)=0
      co(2,2)=2
      co(3,2)=0
      co(1,3)=5
      co(2,3)=2
      co(3,3)=0
      co(1,4)=10
      co(2,4)=2
      co(3,4)=0
      co(1,5)=10
      co(2,5)=0
      co(3,5)=0
      co(1,6)=5
      co(2,6)=0
      co(3,6)=0
      co(1,7)=0
      co(2,7)=0
      co(3,7)=2
      co(1,8)=0
      co(2,8)=2
      co(3,8)=2
      co(1,9)=5
      co(2,9)=2
      co(3,9)=2
      co(1,10)=10
      co(2,10)=2
      co(3,10)=2
      co(1,11)=10
      co(2,11)=0
      co(3,11)=2
      co(1,12)=5
      co(2,12)=0
      co(3,12)=2
      nk=12
! element data
      kon(1)=1
      kon(2)=7
      kon(3)=12
      kon(4)=6
      kon(5)=2
      kon(6)=8
      kon(7)=9
      kon(8)=3
      kon(9)=6
      kon(10)=12
      kon(11)=11
      kon(12)=5
      kon(13)=3
      kon(14)=9
      kon(15)=10
      kon(16)=4
      ipkon(1)=0
      ipkon(2)=8
      lakon(1)='C3D8    '
      lakon(2)='C3D8    '
      nkon=16
      ne=2
      istep=1
! boundary data spc
      do i=1,3
         nodeboun(i)=1
      enddo
      do i=4,6
         nodeboun(i)=2
      enddo
      do i=7,9
         nodeboun(i)=7
      enddo
      do i=10,12
         nodeboun(i)=8
      enddo
      do i=1,12
         if(mod(i,3).eq.0) then
            ndirboun(i)=3
         else
            ndirboun(i)=mod(i,3)
         endif
         xboun(i)=0.0
      enddo
      do i=1,12
         ilboun(i)=i
         ikboun(i)=8*(nodeboun(i)-1)+ndirboun(i)
      enddo
      nboun=12
      do i=1,nboun
         typeboun(i)='B'
      enddo
      ipompc(1)=0
      do i=1,3
         nodempc(i,1)=0
      enddo
      coefmpc(1)=0
      nmpc=0
! node force data
      nodeforc(1,1)=5
      nodeforc(1,2)=11
      do i=1,2
         ndirforc(i)=2
         xforc(i)=10000.
      enddo
      ikforc(1)=34
      ikforc(2)=82
      ilforc(1)=1
      ilforc(2)=2
      nforc=2
      nload=0
      nprint=0
      mpcfree=1
!mei data for frequency
      mei(1)=80     !data for frequency ansys
      mei(2)=0
      mei(3)=32596224
      mei(4)=0
!set data
      set(1)='NALLN'
      set(2)='EALLE'
      istartset(1)=1
      istartset(2)=13
      iendset(1)=12
      iendset(2)=14
      do i=1,14
         ialset(i)=mod(i,12)
      enddo
      ialset(12)=12
      nset=2
      nalset=14
! material elastics data
      elcon(0,1,1)=0
      elcon(1,1,1)=210000.
      elcon(2,1,1)=0.3
      nelcon(1,1)=2
      nelcon(2,1)=1
      alzero(1)=0
      do j=1,nk
         t1(j)=t0(j)
      enddo
! material data and the element material prop
      matname(1)='STEEL'
      ielmat(1,ialset(13))=1
      ielmat(1,ialset(14))=1
      ielorien(1,1)=0
      ielorien(1,2)=0
      amname(1)='\000'
      amname(2)='\000'
      amta(1,1)=0
      amta(1,2)=0
      amta(2,1)=0
      amta(2,2)=0
      namta(1,1)=0
      namta(1,2)=0
      namta(2,1)=0
      namta(2,2)=0
      namta(3,1)=0
      namta(3,2)=0
      nam=0
! calculation type
      nmethod=1
!
      iamforc(1)=0
      iamforc(2)=0
      iamt1(1)=0
      iamt1(2)=0
      fei=(/0,0,0/)
      do i=0,mi(2)
         do j=1,nk
            veold(i,j)=0
         enddo
      enddo
      ithermal=(/0,0/)
      iperturb(1)=0
      iperturb(2)=0
      timepar(1)=0
      timepar(2)=1
      timepar(3)=0
      timepar(4)=0
      timepar(5)=0
! node file ouput options
      nlabel=48
      do i=1,nlabel
         filab(i)='      '
      enddo
      filab(1)='U    G'
      filab(3)='S    G'
      filab(4)='E    G'
!
      jout=(/0,0/)
      idrct=0
      jmax=(/100,10000/)
      iexpl=0
      alpha=0
      do i=1,12
         iamboun(i)=0
      enddo
      ! labmpc(1)='\000'
      ! labmpc(2)='\000'
      nshcon(1)=0
      shcon(0,1,1)=0
      shcon(1,1,1)=0
      shcon(2,1,1)=0
      shcon(3,1,1)=0
      do i=0,6
         cocon(i,1,1)=0
      enddo
      maxlenmpc=0
      do i=0,mi(2)
         vold(i,1)=0
      enddo
      fmpc(1)=0
      nam_=2
      nprop=0
      nprop_=0
      itpamp=0
      iviewfile=0
      idefforc(1)=0
      idefforc(2)=0
      islavsurf(1,1)=0
      islavsurf(2,1)=0
      heading(1)='test'
      memmpcref_=0
      maxlenmpcref=0
      do i=1,6
         do j=1,mi(1)
            prestr(i,j,1)=0
            prestr(i,j,2)=0
         enddo
      enddo
!
      write(*,*)
      write(*,*) 'STEP ',istep
      write(*,*)
      if(nmethod.eq.-1) then
         write(*,*) 'Visco analysis was selected'
      elseif(nmethod.eq.0) then
         write(*,*) 'No analysis was selected'
      elseif(nmethod.eq.1) then
         write(*,*) 'Static analysis was selected'
      elseif(nmethod.eq.2) then
         write(*,*) 'Frequency analysis was selected'
      elseif(nmethod.eq.3) then
         write(*,*) 'Buckling analysis was selected'
      elseif(nmethod.eq.4) then
         write(*,*) 'Dynamic analysis was selected'
      endif
!
      return
      end
