
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
KGE_mean = zeros(5,3); % r, RMSE, KGE
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
    [KGE_mean(j,1), KGE_mean(j,2), KGE_mean(j,3)] = efficiency2_DLC(S1,B(:,2));
    S0(j,1) = S1(1,1);
    S0(j,2) = B(1,2);
end
% save(append('output/mat_' , string(datetime('now', 'format', 'uuuu.MM.dd_HH.mm.ss')), '.mat'), 'S');
% KGE_mean;
fprintf('%s: %f\n','R2',mean(KGE_mean(:,1))^2);
fprintf('%s: %f\n','RMSE',mean(KGE_mean(:,2)));
fprintf('%s: %f\n','KGE',mean(KGE_mean(:,3)));

% figure
% hold on
% plot(DATI(:,5))
% plot(S, 'Color','red')
% FILENAME = [append('output/mat_' , string(datetime('now', 'format', 'uuuu.MM.dd_HH.mm.ss')),  '.jpg')];
% print('-djpeg','-r300',FILENAME);

