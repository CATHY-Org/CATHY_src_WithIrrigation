
% Write Ksoil and Kbedrock in the soil input file for cathy
% Khsoil=10^X(1)/100/3600;
% Kvsoil=Khsoil/X(3);
% Kbedr=Khsoil/10^X(2);
% Sssoil=10^X(4);
% Ssbedr=10^X(5);
%
% SOIL=[Khsoil Khsoil Kvsoil Sssoil 0.58 1.250  0.09  -0.0142];
% BEDR=[Kbedr  Kbedr  Kbedr  Ssbedr 0.33 1.250  0.09  -0.0510];
%ricorda che se usi la trasformata logaritmica qui i parametri vanno back-trasform ati per poterli fornire al cathy

function [KGE] = functn( x )% run cathy and compute KGE for the water levels

global BESTX BESTF ICALL PX PF
% Kx_1 = 10^x(i,1);
% Ky_1 = Kx_1;
% Kz_1 = 10^x(i,2);
% Kx_aqt = 10^x(i,3);
% Ky_aqt = Kx_aqt;
% Kz_aqt = Kx_aqt;
% Kx_2 = 10^x(i,4);
% Ky_2 = Kx_2;
% Kz_2 = 10^x(i,5);
% Kx_3 = 10^x(i,6);
% Ky_3 = Kx_3;
% Kz_3 = 10^x(i,7);
% Kx_4 = 10^x(i,8);
% Ky_4 = Kx_4;
% Kz_4 = 10^x(i,9);

%Kx_1 = 1.88E-09;
Kx_1 = 10^x(1);
Ky_1 = Kx_1;
Kz_1 = Kx_1;
Kx_2 = 10^x(6);
Ky_2 = Kx_2;
Kz_2 = Kx_2;

Ss_1 = 1.00E-03;
Ss_2 = 1.00E-03;

P_1 = x(2);
P_2 = x(7);

N_1 = x(3);
N_2 = x(8);

Thetar_1 = x(4);
Thetar_2 = x(9);

VGPSAT_1 = x(5);
VGPSAT_2 = x(10);

myIC = x(11);

%%tutta l assegnazione dipende da come impostata la stratigrafia

SOIL_I =   [Kx_1 Ky_1 Kz_1 Ss_1 P_1 N_1 Thetar_1 VGPSAT_1];
SOIL_II =  [Kx_2 Ky_2 Kz_2 Ss_2 P_2 N_2 Thetar_2 VGPSAT_2];

%%questi sono  i parametri fissi del file soil
% cd ~/Desktop/SCEUA_Matlab/cathy

fileID = fopen('input/soil','w');
fprintf(fileID,'%.1f\n',-1.0e+20);
fprintf(fileID,'%u %.1f\n',[0 1.0]);
fprintf(fileID,'%.2f %.3f\n',[0.4 0.225]);
fprintf(fileID,'%.2f %.2f %.2f %.3f %.2f %.1f\n',[0.0 -4.0 -150.0 0.2  1.0 1.0]);
fprintf(fileID,'%.2f %.2f %.2f %.3f %.2f %.1f\n',[0.0 -4.0 -150.0 0.2  1.0 1.0]);
fprintf(fileID,'%u\n',0);
fprintf(fileID,'%.2f %.1f %.1f %.1f %.3f\n',[0.02 2 2 0 0.333]);
fprintf(fileID,'%.1f\n',1);
fprintf(fileID,'%.1f %.1f \n',[-5 1]);
fprintf(fileID,'%.1f %.1f %.3f\n',[1.2 0.0 -0.345]);

fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_I);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_I);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_I);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_I); % 10 cm
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_I);

fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_I);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_I);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_I); % 20 cm
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_I);

fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II); % 30 cm
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);

fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II); % 40 cm
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);

fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II); % 50 cm
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);

fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II); % 25 layers
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II);
fprintf(fileID,'%e %e %e %.3g %.3g %.3g %.3g %.3g\n',SOIL_II); % 36 layers


%fprintf(fileID,'%u\n',1);%CHIEDI!!!
%fprintf(fileID,'%u\n',20);
fclose(fileID);


%%questi sono  i parametri fissi del file ic
% 4 0 INDP IPOND
% 2.0   WTPOSITION 

fileID = fopen('input/ic','w');
fprintf(fileID,'%d %d INDP IPOND\n', [4 0]);
fprintf(fileID,'%.2f WTPOSITION\n', [myIC]);
fclose(fileID);

% Run cathy model
delete('vtk/*'); % because I use this to understand whether the simulation ran to the end
[s,w] = system('nohup ./cathy_ft > /dev/null');

%questa parte serve per i valori misurati, che nel nostro caso NON sono sintetici ma sono MISURATI; qui bisogna importarli per fare il calcolo degli errori
%MIS=load('/Users/campo/Work/papers/panola/data/trench.txt');
%J=find(MIS(:,1)>=88.0 & MIS(:,1)<=94.0);
%Ti=MIS(J,1);
%Oi=MIS(J,3);
%questa parte importa i valori MISURATI per fare il calcolo della funzione obiettivo
% Read results and compute RMSE





%questa parte importa gli output di modellazione -> WTDEPTH
%[C,IA,IC] = unique(HG.data(:,3),'last');
%TIM=HG.data(IA,3)./86400+(88.0-49.0);
%QSF=-1000*15*60*HG.data(IA,5);
%if HG.data(end,3)<4752000.0
%   Si=zeros(size(Oi));
%else
%    Si=interp1(TIM,QSF,Ti,'linear');
%end

% cd ~/Desktop/SCEUA_Matlab/cathy


% FileInfo = dir('vtk/*');
% FileInfo_a = append('vtk/', FileInfo(3).name);
% FileInfo_z = append('vtk/', FileInfo(end).name);
% FileInfo_a = 'vtk/100.vtk';
% FileInfo_z = 'vtk/900.vtk'; % to be changed according to how many outputs are saved
% if ( datenum(dir(FileInfo_z).date) > datenum(dir(FileInfo_a).date) )
%%% but if 900 not created, then cannot do it, thus simply check if it is
%%% there
if exist('vtk/388.vtk', 'file') == 2 % this 100 + 2 (time = 0 and tend) + number vtk to save ; or run CATHY and copy name of last file
    disp('Running script extract output')
    cd matlab
    Watertable_pointS_PHIvary_sceua;
    cd ..




    %importo i dati forniti MISURATI
    %CASO_A: watertable

    % cd /Users/anna.botto/Desktop/CATHY/3_progetto_SCEUA/6_SCEUA_cathy_lancio_2K
    % cd ~/Desktop/SCEUA_Matlab

    %% import data observed
    % date from midnight 21 october (simulation started 24 hours before), EC, H
    % EPA SWMM
    % DATI = load("SWC_meas.mat");
    % DATI = cell2mat(struct2cell(DATI));
    % SWC_meas(:,1) = linspace(0,86400*2,size(SWC_meas,1));
    % SWC_meas(:,3) = SWC_meas(:,3) * 1e-3; % from liter per second to cubic meter per second
    % days added to the simulation to forget IC
    % addndays = 7; % plot in days
    % clearvars -except SWC_meas
    % save SWC_meas;

    % % DATI = table2array(readtable('Venezia_2009_goccia_SWall_post.csv','NumHeaderLines',1));
    % DATI(:,1) = DATI(:,1) * 3600; % hours to seconds
    % DATI(:,2) = DATI(:,2) / 100; % from percent to 0-1 like in CATHY
    % 
    % % keep only those saved in CATHY
    % % j = (DATI(:,1) >= 1804.51127819549 & DATI(:,1) <= 811522.784000000) | ...
    % %     (DATI(:,1) >= 1296321.48000000 & DATI(:,1) <= 1784924.75000000) | ...
    % %     (DATI(:,1) >= 2342924.75000000 & DATI(:,1) <= 2825512.05000000);
    % j = (DATI(:,1) <= 5194000);% little bit more than tend parm for interpolation
    % DATI = DATI(j,:);
    
    
    % j = find(TIME(:,2) >= 660600); % before is warm up and no data from sensor
    % TIME = TIME(j,:);
    % mymat = mymat(j,:);

    % it was manually processed to long format
    DATI = readmatrix('R_SWC_long.csv', 'Delimiter',';');
    %j = find(DATI(:,6) >= 660600 & DATI(:,6) <= (10*86400)); % before they are NA & after are not in simulation (0 days warm up and 3 days simulation)
    j = find(DATI(:,6) >= 660600 & DATI(:,6) <= (31*86400)); % before they are NA & after are not in simulation (0 days warm up and 3 days simulation)
    DATI = DATI(j,:);
    DATI(:,6) = DATI(:,6) - 1*86400; % remove warmup that is not run in CATHY

    % a=[0:86400:86400*6, 86400*7:3600:86400*30, 86400*50:3600:86400*67]';
    % % limit to 999 vtk but starts from 100 (+1 for 0 time), so max vtk is 898

    DATI_uni = [];
    S = [];
    % select depths
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% which depth to use?
    KGE_mean = zeros(5,1);
    S0 = zeros(5,2);
    for j=1:5
        switch j
            case 1
                B = DATI(DATI(:,7)==10,[6,5]); % time and data
                B(:,2) = B(:,2); % factor soil texture and sensor
            case 2
                B = DATI(DATI(:,7)==20,[6,5]);
                B(:,2) = B(:,2); % factor soil texture and sensor
            case 3
                B = DATI(DATI(:,7)==30,[6,5]);
                B(:,2) = B(:,2); % factor soil texture and sensor
            case 4
                B = DATI(DATI(:,7)==40,[6,5]);
                B(:,2) = B(:,2); % factor soil texture and sensor
            case 5
                B = DATI(DATI(:,7)==50,[6,5]);
                B(:,2) = B(:,2); % factor soil texture and sensor
        end

        % hours in export could be the same, so unique
        [C,IA,IC] = unique(B(:,1));
        B=B(IA,:);
        %DATI_uni = [DATI_uni;B];
        
        dataSim = mymat(2:end-1,j)*100;
        S1=interp1(TIME(2:end-1,2), dataSim, B(:,1),'linear','extrap'); % sample point, sample values, query points
        % figure
        % hold on
        % plot(TIME(2:end-1,2), mymat(2:end-1,j))
        % plot(B(:,1), B(:,2))
        S = [S;S1];

        % because different profiles, calculate mean KGE
        KGE_mean(j,1) = efficiency2(S1,B(:,2));
        S0(j,1) = S1(1,1);
        S0(j,2) = B(1,2);
    end
    % save(append('output/mat_' , string(datetime('now', 'format', 'uuuu.MM.dd_HH.mm.ss')), '.mat'), 'S');

    figure
    hold on
    plot(DATI(:,5))
    plot(S, 'Color','red')
    FILENAME = [append('output/mat_' , string(datetime('now', 'format', 'uuuu.MM.dd_HH.mm.ss')),  '.jpg')];
    print('-djpeg','-r300',FILENAME);
    close

    % cd /Users/anna.botto/Desktop/CATHY/3_progetto_SCEUA/6_SCEUA_cathy_lancio_2K
    % cd ~/Desktop/SCEUA_Matlab

    % ASSICURARSI CHE S E DATI SIANO DUE VETTORI COLONNA!!!

    copyfile('input/soil' , append('input/soil_' , string(datetime('now', 'format', 'uuuu.MM.dd_HH.mm.ss')))); % save copy soil file
    copyfile('input/ic' , append('input/ic_' , string(datetime('now', 'format', 'uuuu.MM.dd_HH.mm.ss')))); % save copy soil file
    %[KGE]=efficiency2(S,DATI_uni(:,2));
    [KGE] = mean(KGE_mean, 'omitnan');
    fileDLC = fopen('objfun.txt','a');
    fprintf(fileDLC, 'Time %16s and KGE mean is %5.3f. All KGE are: %5.3f %5.3f %5.3f %5.3f %5.3f\n',string(datetime('now', 'format', 'uuuu.MM.dd_HH.mm.ss')),[KGE KGE_mean']);
    fclose(fileDLC);

    clearvars C IA IC B mymat S1 S DATI DATI_uni

else
    disp('CATHY did not run til end')
    %copyfile('input/soil' , append('input/soil_' , string(datetime('now', 'format', 'uuuu.MM.dd_HH.mm.ss')))); % save copy soil file
    %copyfile('input/ic' , append('input/ic_' , string(datetime('now', 'format', 'uuuu.MM.dd_HH.mm.ss')))); % save copy soil file

    [KGE]=+99;
    %fileDLC = fopen('objfun.txt','a');
    %fprintf(fileDLC, 'Time %16s and KGE is %5.3f\n',string(datetime('now', 'format', 'uuuu.MM.dd_HH.mm.ss')),KGE);
    %fclose(fileDLC);
end


end

