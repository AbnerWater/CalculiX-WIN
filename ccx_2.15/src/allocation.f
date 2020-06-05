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
      subroutine allocation(nload,nforc,nboun,nk,ne,nmpc,
     &  nset,nalset,nmat,ntmat,npmat,norien,nam,nprint,
     &  mi,ntrans,set,meminset,rmeminset,ncs,
     &  namtot,ncmat,memmpc,ne1d,ne2d,nflow,jobnamec,irstrt,
     &  ithermal,nener,nstate,irestartstep,inpc,ipoinp,inp,
     &  ntie,nbody,nprop,ipoinpc,nevdamp,npt,nslavs,nkon,mcs,
     &  mortar,ifacecount,nintpoint,infree,nheading,nobject,
     &  iuel,iprestr,nstam,ndamp,nef)
!
      implicit none

      character*1 inpc(*)
      character*81 set(*)
      character*132 jobnamec(*)
!
      integer nload,nforc,nboun,nk,ne,nmpc,nset,nalset,
     &  nmat,ntmat,npmat,norien,nam,nprint,
     &  meminset(*),mi(*),
     &  ntrans,
     &  ncs,rmeminset(*),
     &  namtot,ncmat,memmpc,
     &  ne1d,ne2d,
     &  nflow,irestartstep,
     &  irstrt(*),ithermal(2),nener,nstate,ipoinp(2,*),inp(3,*),
     &  ntie,nbody,nprop,ipoinpc(0:*),nevdamp,npt,
     &  nslavs,nkon,mcs,
     &  ifacecount,nintpoint,mortar,infree(4),nheading,
     &  nobject,
     &  iuel(4,*),
     &  iprestr,nstam,ndamp,nef
!   size input
      nload=0
      nforc=2
      nboun=12
      nk=12
      ne=2
      nmpc=1
      nset=2
      nalset=16
      nmat=1
      ntmat=1
      npmat=0
      norien=0
      nam=2
      nprint=0
      mi(1)=8
      mi(2)=3
      mi(3)=1
      ntrans=0
      set(1)='NALLN'
      set(2)='EALLE'
      meminset(1)=12
      meminset(2)=2
      rmeminset(1)=12
      rmeminset(2)=2
      ncs=0
      namtot=2
      ncmat=2
      memmpc=1
      ne1d=0
      ne2d=0
      nflow=0
      irstrt(1)=0
      irstrt(2)=0
      ithermal(1)=0
      ithermal(2)=0
      nener=0
      nstate=0
      ntie=0
      nbody=0
      nprop=0
      nevdamp=0
      npt=0
      nslavs=0
      nkon=16
      mcs=0
      mortar=0
      ifacecount=0
      nintpoint=0
      infree=(/0,0,0,0/)
      nheading=1
      nobject=0
      iprestr=0
      nstam=0
      ndamp=0
      nef=0
!
      write(*,*)
      write(*,*) ' The numbers below are estimated upper bounds'
      write(*,*)
      write(*,*) ' number of:'
      write(*,*)
      write(*,*) '  nodes: ',nk
      write(*,*) '  elements: ',ne
      write(*,*) '  one-dimensional elements: ',ne1d
      write(*,*) '  two-dimensional elements: ',ne2d
      write(*,*) '  integration points per element: ',mi(1)
      write(*,*) '  degrees of freedom per node: ',mi(2)
      write(*,*) '  layers per element: ',mi(3)
      write(*,*)
      write(*,*) '  distributed facial loads: ',nload
      write(*,*) '  distributed volumetric loads: ',nbody
      write(*,*) '  concentrated loads: ',nforc
      write(*,*) '  single point constraints: ',nboun
      write(*,*) '  multiple point constraints: ',nmpc
      write(*,*) '  terms in all multiple point constraints: ',memmpc
      write(*,*) '  tie constraints: ',ntie
      write(*,*) '  dependent nodes tied by cyclic constraints: ',ncs
      write(*,*) '  dependent nodes in pre-tension constraints: ',npt
      write(*,*)
      write(*,*) '  sets: ',nset
      write(*,*) '  terms in all sets: ',nalset
      write(*,*)
      write(*,*) '  materials: ',nmat
      write(*,*) '  constants per material and temperature: ',ncmat
      write(*,*) '  temperature points per material: ',ntmat
      write(*,*) '  plastic data points per material: ',npmat
      write(*,*)
      write(*,*) '  orientations: ',norien
      write(*,*) '  amplitudes: ',nam
      write(*,*) '  data points in all amplitudes: ',namtot
      write(*,*) '  print requests: ',nprint
      write(*,*) '  transformations: ',ntrans
      write(*,*) '  property cards: ',nprop
      write(*,*)
!
      return
      end