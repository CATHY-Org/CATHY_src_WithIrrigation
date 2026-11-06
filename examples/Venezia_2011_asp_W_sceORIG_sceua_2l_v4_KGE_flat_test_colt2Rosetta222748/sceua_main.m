clear all;
close all
clc;

delete('objfun.txt'); % created in functn.m
fileDLC = fopen('objfun.txt','w');
%fprintf(fileDLC, 'Time %16s and KGE is %5.3f\n',string(datetime('now', 'format', 'uuuu.MM.dd_HH.mm')),KGE);
fclose(fileDLC);

delete('results.mat'); % created here when scripts runs succesfully
% cd /Users/anna.botto/Desktop/CATHY/3_progetto_SCEUA/6_SCEUA_cathy_lancio_2K

% cd ~/Desktop/SCEUA_Matlab

% ordine dei materiali per colonna: kz1,ELSTOR,POROS,VGNCELL,VGRMCCELL,VGPSATCELL

global BESTX BESTF ICALL PX PF

% 
bl = [log10(1.66E-06)   0.15   1.07 0.01 (0.1), ...
      log10(1.66E-06)   0.15   1.07 0.01 (0.1), ...
      1.5];

x0 = [log10(1.66E-05)   0.46   1.37 0.06 (0.43), ...
      log10(1.66E-05)   0.46   1.37 0.06 (0.43), ...
      4.5];

bu = [log10(1.66E-04)   0.55   2.07 0.10 (0.9), ...
      log10(1.66E-04)   0.55   2.07 0.10 (0.9), ...
      6.6];




maxn=210;     % dovrebbe essere almeno 210
kstop=1;     % iterazioni consentite
pcento=0.5;  % tolleranza convergenza
peps=0.01;   % DA DEFINIRSI è una ulteriore tolleranza perchè le fasce non vengano troppo strette
iseed=-1;    % campionamento pseudo random
iniflg=1;    % li mantiene come dati iniziali
ngs=2;       % la p___ questo è un elemento da sensitività perchè legato allo specifico problema


[bestx,bestf] = sceua(x0,bl,bu,maxn,kstop,pcento,peps,ngs,iseed,iniflg);

save -v7 results.mat;
return;
