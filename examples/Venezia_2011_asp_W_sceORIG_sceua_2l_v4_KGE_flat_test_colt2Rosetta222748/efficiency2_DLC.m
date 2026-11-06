function [r,RMSE,KGE]=efficiency2_DLC(S,O)

% added by DLC because soil moisture was always 1 at some point
% and if all same value then corr gives NaN
if (sum(S==1)==size(S,1))
    S(1) = 0.9999;
end

% delete efficiency.m~;
E2=1-(sum((S-O).^2)/sum((O-mean(O)).^2));
D2=1-(sum((S-O).^2)/sum((abs(O-mean(O))+abs(S-mean(O))).^2));
E1=1-(sum(abs(S-O))/sum(abs(O-mean(O))));
D1=1-(sum(abs(S-O))/sum(abs(O-mean(O))+abs(S-mean(O))));
RMSE=(mean((S-O).^2))^0.5;
%NRMSE=RMSE/(max(O)-min(O));
%KGE = NRMSE; % avoid changing things in other scripts
%
r=min(min(corr(S,O)));
a=std(S)/std(O);
b=mean(S)/mean(O);
KGE=1-sqrt((r-1)^2+(a-1)^2+(b-1)^2);
%KGE=1-KGE;
