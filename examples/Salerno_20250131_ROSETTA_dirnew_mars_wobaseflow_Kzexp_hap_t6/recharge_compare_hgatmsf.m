%clear all

% parameters
warmupCATHY = 183; % days. for integration volumes
parmTMAX = 6304200/86400; % days- for calculating integrals
Area = 9.83040E+06;
%
% get current foldername
[~,foldername,~]=fileparts(pwd);

%
fileatmbc = fileread('output/hgatmsf');
tmp = textscan(fileatmbc, '%f %f %f %f %f %f %f %f %f %f', 'Delimiter', '','HeaderLines',1, 'EmptyValue', Inf);
tmp = cell2mat(tmp);
% time is column 3
% recharge flux in columns 9
% recharge volume is 10


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fileatmbc = fileread('../Salerno_20250131_ROSETTA_dirnew_mars_wobaseflow_Kzexp_hap_t6_VEG/output/hgatmsf');
tmpVEG = textscan(fileatmbc, '%f %f %f %f %f %f %f %f %f %f', 'Delimiter', '','HeaderLines',1, 'EmptyValue', Inf);
tmpVEG = cell2mat(tmpVEG);

%% RECHARGE FROM FLUX %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
recharge = tmp(:,9);
j = find(recharge < 0);
recharge(j) = 0;
recharge = recharge./Area.*tmp(:,2);
%
recharge_VEG = tmpVEG(:,9);
j = find(recharge_VEG < 0);
recharge_VEG(j) = 0;
recharge_VEG = recharge_VEG./Area.*tmpVEG(:,2);
%
figure
plot(tmp(:,3)/86400 - 183,cumsum(recharge))
hold on
%xlim([0 350])
plot(tmpVEG(:,3)/86400 - 183,cumsum(recharge_VEG))
xlabel('Day after 1 December 2023')
%ylabel('Cumulative recharge (m^3)');
ylabel('Recharge >0 (m)');
legend('W greenhouse','W/o greenhouse','Location','NorthWest','Box','Off')
set(gca,'FontSize',14)%,'YScale','log')
FILENAME = ['recharge_compare_hgatmsf_positive'];
print('-djpeg','-r300',FILENAME);

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
figure
hold on
plot(tmp(:,3)/86400 - 183,cumsum(tmp(:,9)./Area.*tmp(:,2)))
% xlim([0 350])
plot(tmpVEG(:,3)/86400 - 183,cumsum(tmpVEG(:,9)./Area.*tmpVEG(:,2)))
xlabel('Day after 1 December 2023')
%ylabel('Cumulative recharge (m^3)');
ylabel('Recharge < > 0 (m)');
legend('W greenhouse','W/o greenhouse','Location','NorthWest','Box','Off')
set(gca,'FontSize',14)%,'YScale','log')
FILENAME = ['recharge_compare_hgatmsf_all'];
print('-djpeg','-r300',FILENAME);

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
figure
hold on
plot(tmp(:,3)/86400 - 183,tmp(:,9))
%xlim([0 350])
plot(tmpVEG(:,3)/86400 - 183,tmpVEG(:,9))
xlabel('Day after 1 December 2023')
%ylabel('Cumulative recharge (m^3)');
ylabel('Recharge flux (m3/s)');
legend('W greenhouse','W/o greenhouse','Location','NorthEast','Box','Off')
set(gca,'FontSize',14)%,'YScale','log')
FILENAME = ['recharge_compare_hgatmsf_flux'];
print('-djpeg','-r300',FILENAME);

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
figure
hold on
plot(tmp(:,3)/86400 - 183,tmp(:,10))
% xlim([0 350])
plot(tmpVEG(:,3)/86400 - 183,tmpVEG(:,10))
xlabel('Day after 1 December 2023')
%ylabel('Cumulative recharge (m^3)');
ylabel('Recharge (m) output');
legend('W greenhouse','W/o greenhouse','Location','NorthWest','Box','Off')
set(gca,'FontSize',14)%,'YScale','log')
FILENAME = ['recharge_compare_hgatmsf'];
print('-djpeg','-r300',FILENAME);

