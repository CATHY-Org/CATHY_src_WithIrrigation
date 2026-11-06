%close all
% if ishandle(1), close(1), end % this is because I am using together this script with one for Venezia's data calibration which I number from 4 on to check history of changes
% if ishandle(2), close(2), end
% if ishandle(3), close(3), end
clear all
close all
DOATMPOT = 0;

% WITH THE GIVEN RATING CURVE, BASED ON SWMM ...
% WL IS OVERESTIMATED AND Q IS UNDERESTIMATED ...
% MEANING THAT WL CALCULATED FROM CATHY DISCHARGE IS UNDERESTIMATED
% MEANING THAT Q CALCULATED FROM WL SENSOR IS OVERESTIMATED
% CORRECT?


% remove wrong data measured
myst = 17.3646;
myen = 21.3472;

% parameters
warmupCATHY = 183; % days. for integration volumes
parmTMAX = 6304200/86400; % days- for calculating integrals
offsetsensor = -0/24; % days. this is because rainfall may take a while from battipaglia to the catchment. it is based on matching the water level rise
DOSWMMM = 1; % flag
warmupSWMM = 1; % days. for integration volumes

%
hbasesens = 0.1; % meters distance between bottom and lowest point of sensor. do not consider sediments

hrett = 0.5; % m. depth of rectangulat part of channel. max is 50 cm. consider sediments
hsedgolena = 0.; % m, if sediments
hsedrett = 0.; % m, if sediments

Ks_design=70;
slope_design=0.00105; % or 0.00021

Ks_real = 40;
slope_real = (5.6-4.6)/2570;


%% import data modelled SWMM
% water level is not accurate. Please see notes in read me file in REWATERING/SWMM
% date from midnight
% EPA SWMM
% one value every 15 minutes
if (DOSWMMM==1)
    fid = fopen('SWMM_Q_WL_C3.txt');
    %header = textscan(fid,'%s',3);            % Optionally save header names
    C = textscan(fid,'%d%s%f%f','HeaderLines',4); % Read data skipping header
    fclose(fid);                                % Don't forget to close file

    QSWMM = zeros(size(C{1},1) , 2);
    QSWMM(:,1) =  0:size(QSWMM,1)-1;
    QSWMM(:,1) = QSWMM(:,1) * 15*60 / 86400 - warmupCATHY; % days
    QSWMM(:,2) = C{3};

    WLSWMM = zeros(size(QSWMM));
    WLSWMM(:,1) =  QSWMM(:,1);
    WLSWMM(:,2) = C{4};
    clearvars C
end

% %% import data observed - value every 5 minutes
% 
% Shiny_WL = fileread('../input/Shiny_WL_BC.csv');
% Shiny_WL = strrep(Shiny_WL, 'NA', '');
% tmp = textscan(Shiny_WL, '%s %f %f', 'Delimiter', ';','HeaderLines',1, 'EmptyValue', Inf);
% 
% DateString = string(tmp{1,1});
% formatIn = 'yyyy-mm-dd HH:MM';
% Hmeasured = datenum(DateString,formatIn);
% Hmeasured = Hmeasured - Hmeasured(1,1); % days
% Hmeasured = Hmeasured + offsetsensor;
% 
% Hmeasured = [Hmeasured, cell2mat(tmp(2:3))];

%% normal hgraph script
HG1=importdata('../output/hgraph',' ',2);

Tsim1=HG1.data(:,1)./86400 - warmupCATHY; % days
%Tsim1_pos=datenum(2023,12,04,23,59,0)+HG1.data(:,1)./86400;
[C,IA,IC] = unique(Tsim1);

Tsim1 = Tsim1(IA);

Qsim1=HG1.data(IA,2); % could be second or third column
if (sum(Qsim1)>0)
    fprintf('reading column 2\n')
else
    fprintf('reading column 3\n')
    Qsim1=HG1.data(IA,3); % could be second or third column
end

% HG1.data(find(Qsim1 > 170,1),1)
max(Qsim1)


%%
figure
hold on
plot(QSWMM(:,1),QSWMM(:,2),'g','LineWidth',3)
plot(Tsim1,Qsim1,'LineWidth',2,'Color','k');
myleg = {'SWMM','CATHY'};

legend(myleg,'Location','NorthWest')

grid on
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')





