%close all
% if ishandle(1), close(1), end % this is because I am using together this script with one for Venezia's data calibration which I number from 4 on to check history of changes
% if ishandle(2), close(2), end
% if ishandle(3), close(3), end
clear all

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
hbasesens = 0.05; % meters distance between bottom and lowest point of sensor. consider sediments

hrett = 0.3; % m. depth of rectangulat part of channel. max is 50 cm. consider sediments
hsedgolena = 0.; % m, if sediments
hsedrett = 0.; % m, if sediments

Ks = 40;
slope = (5.6-4.6)/2570;

% %%
% ATMDIAGN = readmatrix('dtcoupling','NumHeaderLines',28);
% timeatm(1)=ATMDIAGN(1,3);
% for i = 2:nstep
%     timeatm(i)=timeatm(i-1)+ATMDIAGN(i-1,2);
% end
% 
% % figure
% timeatm = timeatm/86400 - warmupCATHY;
% myatmout = 1000.*3600.*ATMDIAGN(:,11);
% myatmout = myatmout / max(abs(myatmout)) / 2;
% % stairs(timeatm./86400,myatmout,'k');
%% ATMBC
dtcoupling = fopen('../output/dtcoupling','r');
%Read in the file
a=0;
nstep=0;
ATMDIAGN=[];
while (a~=(-1))              % Find out number of lines and timesteps (nstep)
    a = fgetl(dtcoupling);   % Loops trough dtcoupling until a=-1 end of file
    nstep = nstep+1;
end
a=0;
nstep = nstep-31; % the first 28 lines of file are text and last 3 rows of file are text
%nstep = nstep-(26+1); % Number of timesteps
frewind(dtcoupling);
%for i = 1:24
for i = 1:28
    fgets(dtcoupling);
end
ATMDIAGN = fscanf(dtcoupling,'%g',[22,nstep]); % Read data
ATMDIAGN = ATMDIAGN';
%close ('all');
fclose(dtcoupling)
jmax=max(ATMDIAGN(:,11));
timeatm(1)=ATMDIAGN(1,3);
for i = 2:nstep
    timeatm(i)=timeatm(i-1)+ATMDIAGN(i-1,2);
end

% figure
timeatm = timeatm/86400 - warmupCATHY;
myatmout = 1000.*3600.*ATMDIAGN(:,11);
myatmout = myatmout / max(abs(myatmout)) / 2;
% stairs(timeatm./86400,myatmout,'k');

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
    QSWMM(:,1) = QSWMM(:,1) * 15*60 / 86400; % days
    QSWMM(:,2) = C{3};

    WLSWMM = zeros(size(QSWMM));
    WLSWMM(:,1) =  QSWMM(:,1);
    WLSWMM(:,2) = C{4};
    clearvars C
end

%% import data observed - value every 5 minutes

Shiny_WL = fileread('../input/Shiny_WL_BC.csv');
Shiny_WL = strrep(Shiny_WL, 'NA', '');
tmp = textscan(Shiny_WL, '%s %f %f', 'Delimiter', ';','HeaderLines',1, 'EmptyValue', Inf);

DateString = string(tmp{1,1});
formatIn = 'yyyy-mm-dd HH:MM';
Hmeasured = datenum(DateString,formatIn);
Hmeasured = Hmeasured - Hmeasured(1,1); % days
Hmeasured = Hmeasured + offsetsensor;

Hmeasured = [Hmeasured, cell2mat(tmp(2:3))];

%% import rainfall - CATHY starts from 1 june 2023
Shiny_Rain = fileread('../input/Shiny_Rain_BC_warmup.csv');
Shiny_Rain = strrep(Shiny_Rain, 'NA', '');
tmp = textscan(Shiny_Rain, '%s %f %f %s %f %f', 'Delimiter', ';','HeaderLines',1, 'EmptyValue', Inf);
date_pre_st = "2023-05-31 23:09";
datedel = string(cell2mat(tmp{1}));
j = find(datedel == date_pre_st,1,'first');
Shiny_Rain_date = cell2mat(tmp(6));
Shiny_Rain1 = cell2mat(tmp(5));
Shiny_Rain_date1 = Shiny_Rain_date;
Shiny_Rain1 = Shiny_Rain1(j:end);
Shiny_Rain_date1 = Shiny_Rain_date1(j:end);

Shiny_Rain = fileread('../input/Shiny_Rain_BC.csv');
Shiny_Rain = strrep(Shiny_Rain, 'NA', '');
tmp = textscan(Shiny_Rain, '%s %f %f %s %f %f', 'Delimiter', ';','HeaderLines',1, 'EmptyValue', Inf);
Shiny_Rain_date = cell2mat(tmp(6));
Shiny_Rain = cell2mat(tmp(5));
Shiny_Rain_date = Shiny_Rain_date;

Shiny_Rain_date = [Shiny_Rain_date1; Shiny_Rain_date];
Shiny_Rain_date = (Shiny_Rain_date - min(Shiny_Rain_date))./86400;
Shiny_Rain = [Shiny_Rain1; Shiny_Rain];

a=timeatm + warmupCATHY;
figure
hold on
stairs(a, 1000.*3600.*ATMDIAGN(:,11),'-k','LineWidth',2);
stairs(Shiny_Rain_date,Shiny_Rain,'-c'); % mm/h -> m/s

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

% Qsim1=interp1(Tsim(IA),HG1.data(IA,2),T1);
% [E2,D2,E1,D1,RMSE,NRMSE,KGE]=efficiency(Qsim',Q1)
Qsim1 = Qsim1+0.1;
disp('Qsim1+0.1')

figure
movegui('west');
plot(Tsim1,Qsim1,'LineWidth',1.5);
hold on
stairs(timeatm./86400,myatmout,'k');
% dateaxis('X',2)
set(gca,'FontSize',14)%,'YScale','log')
xtickangle(20)
ylabel('Streamflow (m^3/s)');
legend('Discharge','Rain','Location','NorthWest')
%FILENAME = ['../output/hgraph'];
FILENAME = ['hgraph'];
print('-djpeg','-r300',FILENAME);
close

% save streamflow
ciccio = [Tsim1,Qsim1];
writematrix(ciccio,'Matlab_Streamflow_simulated.txt');

%% ENGELUND - TRAPEZI
hscala = linspace(0.0,3,301)';
%Qscala = zeros(length(hscala),1);

% hsedgolena = 0.10; % m, if sediments
% hsedrett = 0.35; % m, if sediments

% figure
%y0 = 0;
%z0 = 3;
y = linspace(0,1.6, 170);
z = linspace(3,hrett +hsedgolena, 170);
% plot(y,z)
%y1 = 1.6;
%z1 = 0.5;
y = [y (1.6+2.75)];
z = [z hrett+hsedgolena];
% plot(y,z)
%y2a = y1 + 2.70 -0.1;
%z2a = 0.5;
y = [y (1.6+2.75+0.01)];
z = [z 0+hsedrett];
% plot(y,z)
%y2b = y1 + 2.70;
%z2b = 0;
%y3a = y2b + 2.5;
%z3a = 0;
y = [y (1.6+2.75+2.5)];
z = [z 0+hsedrett];
% plot(y,z)
%y3b = y2b + 2.5 + 0.1;
%z3b = 0.5;
y = [y (1.6+2.75+2.5+0.01)];
z = [z hrett+hsedgolena];
% plot(y,z)
%y4 = y3b + 2.70;
%z4 = 0.5;
y = [y (1.6+2.75+2.5+2.75)];
z = [z hrett+hsedgolena];
% plot(y,z)
%y5 = y4 + 1.6;
%z5 = 3;
y = [y linspace((1.6+2.75+2.5+2.75),(1.6+2.75+2.5+2.75+1.6), 170)];
z = [z linspace(hrett+hsedgolena,3, 170)];
%
figure, plot(y,z)
xlabel('Channel section (m)');
ylabel('Water level (m)');
FILENAME = ['Channel_section'];
print('-djpeg','-r300',FILENAME);

%
cosphi = zeros(1,length(y)-1);
dy = zeros(1,length(y)-1);
for i=1:length(y)-1
    dz = abs(z(i+1)-z(i));
    dy(i) = y(i+1)-y(i);
    cosphi(i) = cos(atan(dz/dy(i))); %or dy/(sqrt((dy^2)+(dz^2)))
end





% % test
% ii=30;
% hscala(ii)
% cumsum = 0;
% for i=1:length(y)-1
%     Zp = max(0, hscala(ii) - z(i+1));
%     Z0 = max(0, hscala(ii) - z(i));
%     if (Zp==0 || Z0==0)
%         cumsum = cumsum + 0;
%     else
%         cumsum = cumsum + cosphi(i)^(2/3) * (Z0^(5/3) + Zp^(5/3)) * dy(i)/2;
%     end
% end
% Qscala = sqrt(slope) * Ks * cumsum;
% [M,I] = min(abs(Qscala - QSWMM(:,end)));
% WLSWMM(I,end)

% build h-Q scale
%hscala = linspace(0.0,3,301)';
Qscala = zeros(length(hscala),1);
Wscala = zeros(length(hscala),1);
for ii=1:length(hscala)
    cumsum = 0;
    cumW = 0;
    for i=1:length(y)-1
        Zp = max(0, hscala(ii) - z(i+1));
        Z0 = max(0, hscala(ii) - z(i));
        if (Zp==0 || Z0==0)
            cumsum = cumsum + 0;
        else
            cumsum = sum([cumsum , cosphi(i)^(2/3) * (Z0^(5/3) + Zp^(5/3)) * dy(i)/2] ,"omitnan");
            cumW = cumW + dy(i);
        end
    end
    Qscala(ii) = sqrt(slope) * Ks * cumsum;
    Wscala(ii) = cumW;
end

figure
% plot(Qscala , Wscala)
plot(Qscala , hscala)
xlabel('Water flow (m3/s)');
ylabel('Water level (m)');
FILENAME = ['Q_WL_curve'];
print('-djpeg','-r300',FILENAME);

% %% fit for hap.in parameter
% x = Qscala;
% y = Wscala;
% ft = fittype('a*(x)^(b)');
% startPoints = [10 0];
% f1 = fit(x,y,ft,'Start', startPoints);
% figure
% plot(f1,x,y)
% f1


%% from Q to WL for CATHY
% figure
% movegui('west');
% plot(Tsim1,Qsim1,'LineWidth',1.5);
% hold on
% % dateaxis('X',2)
% set(gca,'FontSize',14)%,'YScale','log')
% xtickangle(20)
% ylabel('Water flows (m3/s)');

% used in orig
%Ks = 60;
%slope = 0.005;

%HECRAS
% Station 7
% Q bed WL slope vel flowarea topwidth
% 15.36 2.52 4.19 0.000427 0.99 16.80 12.39
%Ks = 40;
%slope = 0.000427;
% design
Ks = 40;
slope = 0.00021; % design
slope = (5.6-4.6)/2570; % SWMM

WLsim1 = zeros(length(Qsim1),1);
for i=1:length(Qsim1)
    [M,I] = min(abs(Qsim1(i)-Qscala));
    WLsim1(i) = hscala(I);
end

% if (DOSWMMM==1)
%     WLSWMM = zeros(size(QSWMM,1),1);
%     for i=1:size(QSWMM,1)
%         [M,I] = min(abs(QSWMM(i,2)-Qscala));
%         WLSWMM(i) = hscala(I);
%     end
% end

myleg = {'CATHY','Sensor'};
figure
movegui('west');
plot(Tsim1,WLsim1,'LineWidth',3);
hold on
if (DOSWMMM==1)
    plot(WLSWMM(:,1),WLSWMM(:,2),'LineWidth',1.5, 'Color', 'Green');
    myleg = {'CATHY','SWMM','Sensor'};
end
% dateaxis('X',2)
set(gca,'FontSize',14)%,'YScale','log')
set(gca,'Xlim',[0 ceil(max(Tsim1))+10]);
xtickangle(20)
ylabel('Water level (m)');

% Sensor 1
% plot(Hmeasured(:,1) / 86400, Hmeasured(:,3) + hmaxrett + hbasesens,'LineWidth',1.5, 'Color', 'red');
% Sensor 2
plot(Hmeasured(:,1), Hmeasured(:,3) + hbasesens,'LineWidth',1, 'Color', 'red');
% plot(WLSWMM(:,1), WLSWMM(:,2),'LineWidth',1.5, 'Color', 'green');
legend(myleg,'Location','NorthWest')

% wrong data
jst = find(Hmeasured(:,1) >= myst,1);
jen = find(Hmeasured(:,1) >= myen,1) -1;
plot(Hmeasured(jst:jen,1), Hmeasured(jst:jen,3) + hbasesens,'LineWidth',1, 'Color', 'yellow');

%FILENAME = ['../matlab/hgraph_WaterLevel'];
%close




%%
if (DOSWMMM==1)
    myleg = {'CATHY','SWMM'};
    figure
    plot(Tsim1,WLsim1,'LineWidth',4);
    hold on
    plot(WLSWMM(:,1),WLSWMM(:,2),'LineWidth',2, 'Color', 'Green');
    % dateaxis('X',2)
    set(gca,'FontSize',14)%,'YScale','log')
    set(gca,'Xlim',[0 ceil(max(Tsim1))+10]);
    xtickangle(20)
    xlabel('Day after 1 December 2023')
    ylabel('Water level (m)');
    legend(myleg,'Location','NorthWest')

    % % wrong data
    % jst = find(Hmeasured(:,1) >= myst,1);
    % jen = find(Hmeasured(:,1) >= myen,1) -1;
    % plot(Hmeasured(jst:jen,1), Hmeasured(jst:jen,3) + hbasesens,'LineWidth',1, 'Color', 'yellow');

    FILENAME = ['hgraph_WaterLevel_SWMM'];
    print('-djpeg','-r300',FILENAME);
end



%%
lwd = 4;
figure('units','normalized','outerposition',[0.3 0 0.7 1])
h1=axes;
hold on
l1=line(Tsim1,WLsim1,'LineWidth',lwd,'color',[0.25, 0.25, 0.25]);
l2=line(Hmeasured(:,1), Hmeasured(:,3) + hbasesens,'LineWidth',lwd-1,'color',[0.9290, 0.6940, 0.1250]);
% wrong data
jst = find(Hmeasured(:,1) >= myst,1);
jen = find(Hmeasured(:,1) >= myen,1) -1;
plot(Hmeasured(jst:jen,1), Hmeasured(jst:jen,3) + hbasesens,'LineWidth',1, 'Color', 'yellow');

xlabel('Time (d)');
ylabel('Water level (m)')
set(h1,'Xlim',[0 ceil(max(timeatm))+10]);
set(h1,'Ylim',[0 4]);
grid on
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')
%
h2=axes;
y2 = 1000.*3600.*ATMDIAGN(:,11);
l3=line(timeatm,y2,'color','blue', 'LineWidth',lwd-2);
h2.Position = h1.Position;
set(h2,'YDir','reverse');
set(h2,'Xlim',[0 ceil(max(timeatm))+10]);
set(h2,'Ylim',[0 40]);
set(h2,'YAxisLocation','Right');
set(h2, 'Color', 'None');
set(h2, 'Xtick', []);
ylabel('Atmbc (mm/h)');

legend( [l1;l2;l3] , 'CATHY','Measured','Atmbc','Location','west','Box','off','Color','none', 'FontName','Calibri','FontSize',30);
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')

FILENAME = ['WL_atm'];
print('-djpeg','-r300',FILENAME);

%% efficiency2
clearvars B
B(:,1) = Hmeasured(:,1); % in days
%B(:,2) = Hmeasured(:,3) + hbasesens; % in meters
B(:,2) = Hmeasured(:,3) + 0; % in meters
TF = ~isinf( B(:,2) );
B = B(TF,:);
S=interp1(Tsim1, WLsim1, B(:,1),'linear','extrap'); % sample point, sample values, query points

% wrong data redo because removed inf values in B
jstK = find(B(:,1) >= myst,1);
jenK = find(B(:,1) >= myen,1) -1;

S(jstK:jenK ,:) = [];
B(jstK:jenK ,:) = [];
% because different profiles, calculate mean KGE
% KGE = efficiency2(S,B(:,2));

adjbaseflow = zeros(max(size(B)),1) ;
j1 = find(B(:,1) >= 179, 1, 'first');
j2 = find(B(:,1) >= 228, 1, 'first');
j3 = find(B(:,1) >= 251, 1, 'first');
j4 = find(B(:,1) >= 334, 1, 'first');
h1 = 0.2;
h2 = 0.47;
h3 = 0.5;
h4 = 0.1;
tmp = adjbaseflow(j1:j2);  jj = (j1:j2) - j1; tmp = tmp(1) + (h2-h1)/(j2-j1) * jj; % figure; plot(jj,tmp);
adjbaseflow(j1:j2) = tmp;
tmp = adjbaseflow(j2:j3);  jj = (j2:j3) - j2; tmp = tmp(1) + (h3-h2)/(j3-j2) * jj;
adjbaseflow(j2:j3) = tmp;
tmp = adjbaseflow(j3:j4);  jj = (j3:j4) - j3; tmp = tmp(1) + (h4-h3)/(j4-j3) * jj; % figure; plot(jj,tmp);
adjbaseflow(j3:j4) = tmp;
S = S + adjbaseflow;

KGE = efficiency2(S,B(:,2)); % +0.2
fprintf('KGE = %.2f neglecting influence of baseflow\n',KGE)

figure('units','normalized','outerposition',[0.3 0 0.7 1])
hold on
plot(B(:,1),B(:,2),'LineWidth',4)
%plot(B(:,1),S+0.2,'LineWidth',2)
plot(B(:,1),S+0.0,'LineWidth',2)
legend({'Measured','Simulated (baseflow corrected)'},'Location','NorthWest','Box','off')
xlabel('Day after 1 Dec 2023')
ylabel('WL (m)')
xlim([0 350])
text(250,2.8,['KGE = ', num2str(round(KGE,2))],'FontSize',30,'FontName','Calibri')
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')

FILENAME = ['WL_correct_baseflow'];
print('-djpeg','-r300',FILENAME);


%% from WL to Q for SENSOR
Qsensor = zeros(length(Hmeasured),1);
for i=1:length(Qsensor)
    [M,I] = min(abs((Hmeasured(i,end)+hbasesens)-hscala));
    Qsensor(i) = Qscala(I);
end

% save streamflow
ciccio = [Hmeasured(:,1), Qsensor];
%writematrix(ciccio,'Matlab_Streamflow.txt');

myleg = {'CATHY','Measured2Q'};
figure
movegui('east');
plot(Tsim1,Qsim1,'LineWidth',3);
hold on
% dateaxis('X',2)
set(gca,'FontSize',14)%,'YScale','log')
set(gca,'Xlim',[0 ceil(max(Tsim1))+10]);
xtickangle(20)
ylabel('Streamflow (m^3/s)');

if (DOSWMMM==1)
    plot(QSWMM(:,1), QSWMM(:,2),'LineWidth',1.5, 'Color', 'green');
    myleg = {'CATHY','SWMM','Measured2Q'};
end
plot(Hmeasured(:,1), Qsensor,'LineWidth',1, 'Color', 'red');

% wrong data
jst = find(Hmeasured(:,1) >= myst,1);
jen = find(Hmeasured(:,1) >= myen,1) -1;
plot(Hmeasured(jst:jen,1), Qsensor(jst:jen),'LineWidth',1, 'Color', 'yellow');

legend(myleg,'Location','NorthWest')

%FILENAME = ['../matlab/hgraph_Discharge'];
FILENAME = ['hgraph_Discharge'];
print('-djpeg','-r300',FILENAME);





%%
lwd = 4;
figure('units','normalized','outerposition',[0.3 0 0.7 1])
h1=axes;
hold on
l1=line(Tsim1,Qsim1,'LineWidth',lwd,'color',[0.25, 0.25, 0.25]);
l2=line(Hmeasured(:,1), Qsensor,'LineWidth',lwd-2,'color',[0.9290, 0.6940, 0.1250]);
% wrong data
jst = find(Hmeasured(:,1) >= myst,1);
jen = find(Hmeasured(:,1) >= myen,1) -1;
plot(Hmeasured(jst:jen,1), Qsensor(jst:jen),'LineWidth',1, 'Color', 'yellow');

xlabel('Time (d)');
ylabel('Streamflow (m³/s)')
set(h1,'Xlim',[0 ceil(max(timeatm))+10]);
set(h1,'Ylim',[0 30]);
grid on
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')
%
h2=axes;
y2 = 1000.*3600.*ATMDIAGN(:,11);
l3=line(timeatm,y2,'color','blue', 'LineWidth',lwd);
set(h2,'YDir','reverse');
set(h2,'Xlim',[0 ceil(max(timeatm))+10]);
set(h2,'Ylim',[0 30]);
set(h2,'YAxisLocation','Right');
set(h2, 'Color', 'None');
set(h2, 'Xtick', []);
ylabel('Atmbc (mm/h)');

legend( [l1;l2;l3] , 'CATHY','Measured2Q','Atmbc','Location','west','Box','off','Color','none', 'FontName','Calibri','FontSize',30);
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')

FILENAME = ['Q_atm'];
print('-djpeg','-r300',FILENAME);


%% efficiency2
clearvars B
B(:,1) = Hmeasured(:,1); % in days
B(:,2) = Qsensor; % in meters
TF = ~isinf( B(:,2) );
B = B(TF,:);
S=interp1(Tsim1, Qsim1, B(:,1),'linear','extrap'); % sample point, sample values, query points

S(jst:jen ,:) = [];
B(jst:jen ,:) = [];
% because different profiles, calculate mean KGE
KGE = efficiency2(S,B(:,2));
fprintf('KGE = %.2f\n',KGE)

return
%% volume water
x = ["CATHY" "CTD" "SWMM" "Rain"];
%x= [1 2 3 4];
% x = ["CATHY" "CTD" "Rain"];
%x= [1 2 3]

fprintf('CATHY until TMAX\n')
j = find(Tsim1 <= parmTMAX);
y = trapz(Tsim1(j)*86400 , Qsim1(j)); % total volume over event in m3
% SWMM is sum([50 , 203 , 354 , 209]) * 10E+6 % litres
% or
% sum([50 , 203 , 354 , 209]) * 10E+3 % m3

fprintf('CTD until TMAX\n')
j = find(Hmeasured(:,1) <= parmTMAX);
y = [y , trapz(Hmeasured(j,1)*86400 , Qsensor(j))]; % total volume over event in m3

fprintf('EPA SWMM unitl TMAX\n')
j = find(QSWMM(:,1) <= parmTMAX+warmupSWMM);
y = [y , trapz(QSWMM(j,1)*86400 , QSWMM(j,end))]; % total volume over event in m3

% rain
% AREATOT (TOTAL CATCHMENT SURFACE AREA)  =     9.83040E+06
% (SURFACE AREA FROM NCELL*DELTAX*DELTAY) =     9.83040E+06
area_Catch = 10150828; %m2
area_Catch = 9.83040E+06; %m2
raintot = sum(Shiny_Rain) / 1000; %mm to m

fprintf(['Volume rain\n'])
y = [y , raintot*area_Catch]

figure
movegui('south');
bar(x,y)
close



