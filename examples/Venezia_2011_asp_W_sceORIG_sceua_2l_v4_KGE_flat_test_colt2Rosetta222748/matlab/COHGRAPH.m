%COHGSFDET: Processes HGRAPH and creates graph
%Open the output file dtcoupling
dtcoupling = fopen('/Users/campo/Work/codes/CATHY/cathy-1.0/transport/beta/runs/weilletal/output/dtcoupling','r');
%Read in the file
a=0;
nstep=0;
ATMDIAGN=[];
while (a~=(-1))              % Find out number of lines and timesteps (nstep)
    a = fgetl(dtcoupling);   % Loops trough dtcoupling until a=-1 end of file
    nstep = nstep+1;
end
a=0;
nstep = nstep-31; % Number of timesteps
frewind(dtcoupling);
for i = 1:28
    fgets(dtcoupling);
end
ATMDIAGN = fscanf(dtcoupling,'%g',[22,nstep]); % Read data
ATMDIAGN = ATMDIAGN';
close ('all');
jmax=max(ATMDIAGN(:,11));
timeatm(1)=ATMDIAGN(1,3);
for i = 2:nstep
    timeatm(i)=timeatm(i-1)+ATMDIAGN(i-1,2);
end

hgraph = fopen('/Users/campo/Work/codes/CATHY/cathy-1.0/transport/beta/runs/weilletal/output/hgraph','r');

% % Read in the file
% a=0;
% nstep=0;
% HGGRAPH=[];
% while (a~=(-1))               % Find out number of lines an timesteps (nstep)
%     a = fgetl(hgraph);        % Loops trough hgsfdet until a=-1 end of file
%     nstep = nstep+1;
% end
% nstep = nstep-2; % Number of timesteps
% frewind(hgraph);
fgets(hgraph);
fgets(hgraph);
HGRAPH = fscanf(hgraph,'%g',[7,nstep]); % Read data: format becomes [7,nstep] with the new version
HGRAPH = HGRAPH';
close('all')

% Plot the graph
%% figure(1);
%% subplot(2,1,1)
%% plot(HGSFDET(:,3),HGSFDET(:,4))
%% grid on
%% xlabel('Time (hours)')
%% ylabel('Netto NANSFDir Flow Volume (m^3)')
%% subplot(2,1,2)
%% plot(HGSFDET(:,3),-HGSFDET(:,5))
h(1) = axes('Position',[0.1 0.8 0.8 .1]);
stairs(timeatm,ATMDIAGN(:,11),'k');
hold on
set(h(1),...
    'XAxisLocation','top',...
    'YAxisLocation','right');%,...
%    'YDir','reverse');
%    'YLim',[0 ceil(jmax*1000/100)*100],...
ylabel('atm. forcing (m/s)');
grid on;
%xlabel('Time (d)');
h(2) = axes('Position',[0.1 0.1 0.8 0.7]);
% [AX,H1,H2]=plotyy(HGRAPH(:,1),HGRAPH(:,2),HGRAPH(:,1),HGRAPH(:,3));
plot(HGRAPH(:,1),HGRAPH(:,2));
grid on
xlabel('time (s)')
% ylim([0 2]);
% set(get(AX(1),'Ylabel'),'String','Discharge (m^3/h)')
% set(AX(1),'YLim',[0 4.0])
% set(AX(2),'YLim',[0 0.8])
% set(get(AX(2),'Ylabel'),'String','Max Courant number')
% set(H1,'LineStyle','-')
% set(H2,'LineStyle',':')
ylabel('streamflow (m^3/s)')
% print('-djpeg','-r300','E:\INRS\papers\cathy-2\brisy\jpg\graphhgraph');
