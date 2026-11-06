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

%%
if (DOATMPOT==1)
    fid = fopen('../output/hgirrigation');
    %header = textscan(fid,'%s',3);            % Optionally save header names
    C = textscan(fid,'%d%f%f%f%f%f%f%f%f%f%f','HeaderLines',14); % Read data skipping header
    fclose(fid);                                % Don't forget to close file

    ATMPOT = zeros(size(C{1},1) , 4);
    ATMPOT(:,1) =  C{2}; %dt (s)
    ATMPOT(:,2) = C{3}; % time (s)
    ATMPOT(:,3) = C{4}; % ATMPOT (mm/h)
    ATMPOT(:,4) = C{7}; % unused irrigation (mm/h)
    clearvars C

    %
    % figure
    % plot(ATMPOT(:,2)/86400-183,ATMPOT(:,3),'-k');
    % xlim([0 350])
    %
    figure
    plot(ATMPOT(:,2)/86400-183,ATMPOT(:,4),'-k');
    xlim([0 350])
    xlabel('Day after 1 December 2023')
    ylabel('Irr not used (L3/T)');
    set(gca,'FontSize',14)%,'YScale','log')
    FILENAME = ['hgirrigation_irrnotused'];
    print('-djpeg','-r300',FILENAME);

end
%%
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
%myatmout = myatmout / max(abs(myatmout)) / 2;
figure
% stairs(timeatm,myatmout,'k');
plot(timeatm,myatmout,'-k');
xlim([0 350])
xlabel('Day after 1 December 2023')
ylabel('Potential atmospheric forcing (L/T)');
set(gca,'FontSize',14)%,'YScale','log')
FILENAME = ['dtcoupling'];
print('-djpeg','-r300',FILENAME);


%% import measured rainfall data
% one value every 15 minutes
DORAIN = 1;
if (DORAIN==1)
    fid = fopen('../input/Shiny_Rain_BC.csv');
    C = textscan(fid,'%s%f%f%s%f%f','HeaderLines',1,'Delimiter',';','TreatAsEmpty','NA'); % Read data skipping header
    fclose(fid);                                % Don't forget to close file

    cRAIN = 5;
    RAIN = zeros(size(C{cRAIN},1) , 2);
    RAIN(:,1) =  0:size(RAIN,1)-1;
    RAIN(:,1) = RAIN(:,1) * 60*60 / 86400 - 0; %warmupCATHY; % days
    RAIN(:,2) = C{cRAIN};

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
% Qsim1=interp1(Tsim(IA),HG1.data(IA,2),T1);
% [E2,D2,E1,D1,RMSE,NRMSE,KGE]=efficiency(Qsim',Q1)


% %% ENGELUND - TRAPEZI
% hscala = linspace(0.0,2.5,301)';
% disp(sqrt(2.4^2+1.6^2))
% hrett=0.5;
% hsedrett=0.0;
% hsedgolena = 0.0; % m, if sediments
% 
% % figure
% %y0 = 0;
% %z0 = 3;
% y = linspace(0,1.9, 170);
% z = linspace(2.5,hrett +hsedgolena + 0.11, 170);
% % plot(y,z)
% %y1 = 1.6;
% %z1 = 0.5;
% y = [y linspace(1.9,1.9+1.9, 10)];
% z = [z linspace(hrett+hsedgolena+0.11,hrett, 10)];
% % plot(y,z)
% %y2a = y1 + 2.70 -0.1;
% %z2a = 0.5;
% y = [y (1.9+1.9+0.54)];
% z = [z hsedrett];
% % plot(y,z)
% %y2b = y1 + 2.70;
% %z2b = 0;
% %y3a = y2b + 2.5;
% %z3a = 0;
% y = [y (1.9+1.9+0.54+2.5)];
% z = [z hsedrett];
% % plot(y,z)
% %y3b = y2b + 2.5 + 0.1;
% %z3b = 0.5;
% y = [y linspace(1.9+1.9+0.54+2.5,1.9+1.9+0.54+2.5+0.54, 10)];
% z = [z linspace(hsedrett , hrett, 10)];
% % plot(y,z)
% %y4 = y3b + 2.70;
% %z4 = 0.5;
% y = [y linspace(1.9+1.9+0.54+2.5+0.54,1.9+1.9+0.54+2.5+0.54+1.9, 10)];
% z = [z linspace(hrett, hrett+hsedgolena+0.11, 10)];
% % plot(y,z)
% %y5 = y4 + 1.6;
% %z5 = 3;
% y = [y linspace(1.9+1.9+0.54+2.5+0.54+1.9,11.2, 170)];
% z = [z linspace(hrett+hsedgolena+0.11,2.5, 170)];


% with sediments
% figure
hscala = linspace(0.0,2.25,301)';
%y0 = 0;
%z0 = 3;
y = linspace(0,1.9, 170);
z = linspace(2.25,.35, 170);
% plot(y,z)
%y1 = 1.6;
%z1 = 0.5;
y = [y linspace(1.9,1.9+1.9, 10)];
z = [z linspace(.35,.25, 10)];
% plot(y,z)
%y2a = y1 + 2.70 -0.1;
%z2a = 0.5;
y = [y (1.9+1.9+0.27)];
z = [z 0];
% plot(y,z)
%y2b = y1 + 2.70;
%z2b = 0;
%y3a = y2b + 2.5;
%z3a = 0;
y = [y (1.9+1.9+0.27+3.0)];
z = [z 0];
% plot(y,z)
%y3b = y2b + 2.5 + 0.1;
%z3b = 0.5;
y = [y linspace(1.9+1.9+0.27+3.0,1.9+1.9+0.27+3.0+0.27, 10)];
z = [z linspace(0 , .25, 10)];
% plot(y,z)
%y4 = y3b + 2.70;
%z4 = 0.5;
y = [y linspace(1.9+1.9+0.27+3.0+0.27,1.9+1.9+0.27+3.0+0.27+1.9, 10)];
z = [z linspace(.25, .35, 10)];
% plot(y,z)
%y5 = y4 + 1.6;
%z5 = 3;
y = [y linspace(1.9+1.9+0.27+3.0+0.27+1.9,11.2, 170)];
z = [z linspace(.35,2.25, 170)];
%
[C,IA,IC] = unique(y);
y = y(IA);
z = z(IA);
%
figure, plot(y,z)
hold on
plot([0,11.2],[hsedrett,hsedrett], 'k','LineWidth',2)
xlabel('Channel section (m)');
ylabel('Water level (m)');
FILENAME = ['Channel_section_test'];
print('-djpeg','-r300',FILENAME);

%
cosphi = zeros(1,length(y)-1);
dy = zeros(1,length(y)-1);
for i=1:length(y)-1
    dz = abs(z(i+1)-z(i));
    dy(i) = y(i+1)-y(i);
    cosphi(i) = cos(atan(dz/dy(i))); %or dy/(sqrt((dy^2)+(dz^2)))
end






%%

Ks=Ks_design;
slope=slope_design;
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


Ks=Ks_real;
slope=slope_real;
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

hold on
% plot(Qscala , Wscala)
plot(Qscala , hscala)
legend({'Design','Real'})

xlabel('Water flow (m3/s)');
ylabel('Water level (m)');
FILENAME = ['Q_WL_curve_test'];
print('-djpeg','-r300',FILENAME);

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

WLsim1 = zeros(length(Qsim1),1);
for i=1:length(Qsim1)
    [M,I] = min(abs(Qsim1(i)-Qscala));
    WLsim1(i) = hscala(I);
end

if (DOSWMMM==1)
    WLSWMM_curve = zeros(size(QSWMM,1),1);
    for i=1:size(QSWMM,1)
        [M,I] = min(abs(QSWMM(i,2)-Qscala));
        WLSWMM_curve(i) = hscala(I);
    end
end
figure
hold on
plot(WLSWMM(:,1),WLSWMM(:,2),'k','LineWidth',2)
plot(WLSWMM(:,1),WLSWMM_curve(:,1),'r','LineWidth',1)

%%
lwd = 4;
figure('units','normalized','outerposition',[0.3 0 0.7 1])
h1=axes;
hold on
l1=line(Tsim1,WLsim1,'LineWidth',lwd,'color',[0.25, 0.25, 0.25]);
l2=line(Hmeasured(:,1), Hmeasured(:,3) + hbasesens,'LineWidth',lwd-1,'color',[0.9290, 0.6940, 0.1250]);
if (DOSWMMM==1)
    l3=line(QSWMM(:,1), WLSWMM(:,2),'LineWidth',1.5, 'Color', 'green');
    myleg = {'CATHY','Measured2Q','SWMM'};
end
% wrong data
jst = find(Hmeasured(:,1) >= myst,1);
jen = find(Hmeasured(:,1) >= myen,1) -1;
plot(Hmeasured(jst:jen,1), Hmeasured(jst:jen,3) + hbasesens,'LineWidth',1, 'Color', 'yellow');

xlabel('Time (d)');
ylabel('Water level (m)')
set(h1,'Xlim',[0 ceil(max(timeatm))+10]);
set(h1,'Ylim',[0 3]);
grid on
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')
legend(myleg)


FILENAME = ['WL_atm_test'];
print('-djpeg','-r300',FILENAME);
%%
% efficiency2
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
text(150,2.,['KGE = ', num2str(round(KGE,2))],'FontSize',30,'FontName','Calibri')
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')

FILENAME = ['WL_correct_baseflow_test'];
print('-djpeg','-r300',FILENAME);





%% trial 2
% efficiency2
clearvars B
B(:,1) = Hmeasured(:,1); % in days
%B(:,2) = Hmeasured(:,3) + hbasesens; % in meters
B(:,2) = Hmeasured(:,3) + 0; % in meters
TF = ~isinf( B(:,2) );
B = B(TF,:);
S=interp1(Tsim1, Qsim1, B(:,1),'linear','extrap'); % sample point, sample values, query points

% wrong data redo because removed inf values in B
jstK = find(B(:,1) >= myst,1);
jenK = find(B(:,1) >= myen,1) -1;

S(jstK:jenK ,:) = [];
B(jstK:jenK ,:) = [];
% because different profiles, calculate mean KGE
% KGE = efficiency2(S,B(:,2));

adjbaseflow = zeros(max(size(B)),1) ;
j0 = find(B(:,1) >= 000, 1, 'first');
j1 = find(B(:,1) >= 179, 1, 'first');
j2 = find(B(:,1) >= 228, 1, 'first');
j3 = find(B(:,1) >= 251, 1, 'first');
j4 = find(B(:,1) >= 270, 1, 'first');
j5 = find(B(:,1) >= 334, 1, 'first');
h0 = 0.2;
h1 = 0.4;
h2 = 1.0;
h3 = 1.2;
h4 = 0.7;
h5 = 0.25;
h6 = 0.05;
adjbaseflow(j0:j1) = h0;
tmp = adjbaseflow(j1:j2);  jj = (j1:j2) - j1; tmp = tmp(1) + (h2-h1)/(j2-j1) * jj; % figure; plot(jj,tmp);
adjbaseflow(j1:j2) = tmp;
tmp = adjbaseflow(j2:j3);  jj = (j2:j3) - j2; tmp = tmp(1) + (h3-h2)/(j3-j2) * jj;
adjbaseflow(j2:j3) = tmp;
tmp = adjbaseflow(j3:j4);  jj = (j3:j4) - j3; tmp = tmp(1) + (h4-h3)/(j4-j3) * jj; % figure; plot(jj,tmp);
adjbaseflow(j3:j4) = tmp;
tmp = adjbaseflow(j4:j5);  jj = (j4:j5) - j4; tmp = tmp(1) + (h5-h4)/(j5-j4) * jj; % figure; plot(jj,tmp);
adjbaseflow(j4:j5) = tmp;
adjbaseflow(j5:end) = h6;
S = S + adjbaseflow;

WLsim1_baseflow = zeros(length(S),1);
for i=1:length(S)
    [M,I] = min(abs(S(i)-Qscala));
    WLsim1_baseflow(i) = hscala(I);
end
S=WLsim1_baseflow;
KGE = efficiency2(S,B(:,2)); % +0.2
fprintf('KGE = %.2f baseflow corrected\n',KGE)



figure('units','normalized','outerposition',[0.3 0 0.7 1])
hold on
plot(B(:,1),B(:,2),'LineWidth',4)
%plot(B(:,1),S+0.2,'LineWidth',2)
plot(B(:,1),S,'LineWidth',2)
legend({'Measured','Simulated (baseflow corrected)'},'Location','NorthWest','Box','off')
xlabel('Day after 1 Dec 2023')
ylabel('WL (m)')
xlim([0 350])
text(10,1.9,['KGE = ', num2str(round(KGE,2))],'FontSize',30,'FontName','Calibri')
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')

FILENAME = ['WL_correct_baseflow_test_trial2'];
print('-djpeg','-r300',FILENAME);

%% from WL to Q for SENSOR
Qsensor = zeros(length(Hmeasured),1);
for i=1:length(Qsensor)
    [M,I] = min(abs((Hmeasured(i,end)+hbasesens)-hscala));
    Qsensor(i) = Qscala(I);
end

if (DOSWMMM == 1)
    QSWMM_curve = zeros(length(QSWMM(:,1)),1);
    for i=1:length(QSWMM_curve)
        [M,I] = min(abs((WLSWMM(i,2))-hscala));
        QSWMM_curve(i) = Qscala(I);
    end
end
figure
hold on
plot(QSWMM(:,1),QSWMM(:,2),'k','LineWidth',2)
plot(QSWMM(:,1),QSWMM_curve(:,1),'r','LineWidth',1)

% save streamflow
ciccio = [Hmeasured(:,1), Qsensor];
%writematrix(ciccio,'Matlab_Streamflow.txt');

myleg = {'CATHY','Measured2Q'};
figure('units','normalized','outerposition',[0.3 0 0.7 1])
movegui('east');
plot(Tsim1,Qsim1,'LineWidth',4,'Color','k');
hold on
% dateaxis('X',2)
set(gca,'Xlim',[0 ceil(max(Tsim1))+10]);
xtickangle(0)
xlabel('Days')
ylabel('Streamflow (m^3/s)');

plot(Hmeasured(:,1), Qsensor,'LineWidth',3, 'Color', [0.9290, 0.6940, 0.1250]);

if (DOSWMMM==1)
    plot(QSWMM(:,1), QSWMM(:,2),'LineWidth',1.5, 'Color', 'green');
    myleg = {'CATHY','Measured2Q','SWMM'};
end

% wrong data
jst = find(Hmeasured(:,1) >= myst,1);
jen = find(Hmeasured(:,1) >= myen,1) -1;
plot(Hmeasured(jst:jen,1), Qsensor(jst:jen),'LineWidth',1, 'Color', 'yellow');

legend(myleg,'Location','NorthWest')

xlim([0 450])

grid on
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')

%FILENAME = ['../matlab/hgraph_Discharge'];
FILENAME = ['hgraph_Discharge_test'];
print('-djpeg','-r300',FILENAME);




%% trial 2
% efficiency2
clearvars B
B(:,1) = Hmeasured(:,1); % in days
%B(:,2) = Hmeasured(:,3) + hbasesens; % in meters
B(:,2) = Qsensor; % in m3/s
TF = ~isinf( B(:,2) );
B = B(TF,:);
S=interp1(Tsim1, Qsim1, B(:,1),'linear','extrap'); % sample point, sample values, query points

% wrong data redo because removed inf values in B
jstK = find(B(:,1) >= myst,1);
jenK = find(B(:,1) >= myen,1) -1;

S(jstK:jenK ,:) = [];
B(jstK:jenK ,:) = [];

% because different profiles, calculate mean KGE
% KGE = efficiency2(S,B(:,2));

adjbaseflow = zeros(max(size(B)),1) ;
j0 = find(B(:,1) >= 000, 1, 'first');
j01 = find(B(:,1) >= 125, 1, 'first');
j1 = find(B(:,1) >= 166, 1, 'first');
j2 = find(B(:,1) >= 228, 1, 'first');
j3 = find(B(:,1) >= 241, 1, 'first');
j04 = find(B(:,1) >= 252, 1, 'first');
j4 = find(B(:,1) >= 270, 1, 'first');
j5 = find(B(:,1) >= 334, 1, 'first');
h0 = 0.3;
h01 = 0.2;
h1 = 0.1;
h2 = 1.2;
h3 = 1.2;
h04 = 1.5;
h4 = 0.6;
h5 = 0.25;
h6 = 0.15;
adjbaseflow(j0:j01) = h0;
tmp = adjbaseflow(j01:j1);  jj = (j01:j1) - j01; tmp = tmp(1) + (h1-h01)/(j1-j01) * jj; % figure; plot(jj,tmp);
adjbaseflow(j01:j1) = tmp;
tmp = adjbaseflow(j1:j2);  jj = (j1:j2) - j1; tmp = tmp(1) + (h2-h1)/(j2-j1) * jj; % figure; plot(jj,tmp);
adjbaseflow(j1:j2) = tmp;
tmp = adjbaseflow(j2:j3);  jj = (j2:j3) - j2; tmp = tmp(1) + (h3-h2)/(j3-j2) * jj;
adjbaseflow(j2:j3) = tmp;
tmp = adjbaseflow(j3:j04);  jj = (j3:j04) - j3; tmp = tmp(1) + (h04-h3)/(j04-j3) * jj; % figure; plot(jj,tmp);
adjbaseflow(j3:j04) = tmp;
tmp = adjbaseflow(j04:j4);  jj = (j04:j4) - j04; tmp = tmp(1) + (h4-h04)/(j4-j04) * jj; % figure; plot(jj,tmp);
adjbaseflow(j04:j4) = tmp;
tmp = adjbaseflow(j4:j5);  jj = (j4:j5) - j4; tmp = tmp(1) + (h5-h4)/(j5-j4) * jj; % figure; plot(jj,tmp);
adjbaseflow(j4:j5) = tmp;
adjbaseflow(j5:end) = h6;
S = S + adjbaseflow;

j = find(B(:,2) ==0);
B(j,2) = nan;
S(j,1) = nan;

KGE = efficiency2(S,B(:,2)); % +0.2
fprintf('KGE = %.2f baseflow corrected\n',KGE)



figure('units','normalized','outerposition',[0.3 0 0.7 1])
hold on
plot(B(:,1),B(:,2),'LineWidth',4)
plot(B(:,1),S,'LineWidth',2)
legend({'Measured','Simulated (baseflow corrected)'},'Location','NorthWest','Box','off')
xlabel('Time (Days after 1^{st} December 2023)')
ylabel('Q (m^3/s)')
xlim([0 450])
text(250,23.5,['KGE = ', num2str(round(KGE,2))],'FontSize',30,'FontName','Calibri')
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')

FILENAME = ['Q_correct_baseflow_test_trial2'];
print('-djpeg','-r300',FILENAME);


%%
lwd = 4;
figure('units','normalized','innerposition',[0 0.1 1.0 0.9])
h1=axes;
hold on
l1=line(B(:,1),B(:,2),'LineWidth',lwd,'color',[0.25, 0.25, 0.25]);
l2=line(B(:,1),S,'LineWidth',lwd-2,'color',[0.9290, 0.6940, 0.1250]);
% wrong data
%jst = find(Hmeasured(:,1) >= myst,1);
%jen = find(Hmeasured(:,1) >= myen,1) -1;
%plot(Hmeasured(jst:jen,1), Hmeasured(jst:jen,3) + hbasesens,'LineWidth',1, 'Color', 'yellow');

xlabel('Time (Days after 1^{st} December 2023)')
ylabel('Streamflow (m^3s^{-1})')
set(h1,'Xlim',[0 450]);
set(h1,'Ylim',[0 30]);
grid on
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')
%
h2=axes;
y2 = 1000.*3600.*ATMDIAGN(:,11);
l3=line(timeatm,y2/2,'color','blue', 'LineWidth',lwd-2);
h2.Position = h1.Position;
set(h2,'YDir','reverse');
set(h2,'Xlim',[0 450]);
set(h2,'Ylim',[0 30]);
set(h2,'YAxisLocation','Right');
set(h2, 'Color', 'None');
set(h2, 'Xtick', []);
ylabel('P-ET_C^{OF} (mm h^{-1})');
yticks([0 5 10])
yticklabels({'0','10','20'})

legend( [l1;l2;l3] , 'Measured','CATHY','Atmospheric conditions','Position',[0.3 0.55 0.1 0.2],'Box','off','Color','none', 'FontName','Calibri','FontSize',30);
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')

FILENAME = ['Q_correct_baseflow_test_trial2_atm'];
print('-djpeg','-r300',FILENAME);





%%
lwd = 4;
figure('units','normalized','innerposition',[0 0.1 1.0 0.9])
h1=axes;
hold on
l1=line(B(:,1),B(:,2),'LineWidth',lwd,'color',[0.25, 0.25, 0.25]);
l2=line(B(:,1),S,'LineWidth',lwd-2,'color',[0.9290, 0.6940, 0.1250]);
% wrong data
%jst = find(Hmeasured(:,1) >= myst,1);
%jen = find(Hmeasured(:,1) >= myen,1) -1;
%plot(Hmeasured(jst:jen,1), Hmeasured(jst:jen,3) + hbasesens,'LineWidth',1, 'Color', 'yellow');

xlabel('Time (Days after 1^{st} December 2023)')
ylabel('Streamflow (m^3s^{-1})')
set(h1,'Xlim',[0 450]);
set(h1,'Ylim',[0 30]);
grid on
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')
%
h2=axes;
y2 = RAIN(:,2);
l3=line(RAIN(:,1),y2/2,'color','blue', 'LineWidth',lwd-2);
h2.Position = h1.Position;
set(h2,'YDir','reverse');
set(h2,'Xlim',[0 450]);
set(h2,'Ylim',[0 30]);
set(h2,'YAxisLocation','Right');
set(h2, 'Color', 'None');
set(h2, 'Xtick', []);
ylabel('P (mm h^{-1})');
yticks([0 5 10])
yticklabels({'0','10','20'})

legend( [l1;l2;l3] , 'Measured','CATHY','Atmospheric conditions','Position',[0.3 0.55 0.1 0.2],'Box','off','Color','none', 'FontName','Calibri','FontSize',30);
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')

FILENAME = ['Q_correct_baseflow_test_trial2_RAIN'];
print('-djpeg','-r300',FILENAME);