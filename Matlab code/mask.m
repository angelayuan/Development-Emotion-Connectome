load('bs_network_fear_759s.mat', 'bs_network');
mn = mean(bs_network,3);

load('bs_network_fear_759s_agepos.mat', 'nbs');
t_mask = nbs.NBS.test_stat;
t_mask(t_mask<3) = 0;
figure; imagesc(t_mask)

mn(t_mask<3) = 0;
figure; imagesc(mn);


aaa = zeros(32,32);
count = 1;
for col=1:32
    for row=1:32
        aaa(row,col) = count;
        count = count+1;
    end
end
bbb = tril(aaa, -1);
bbb = reshape(bbb,32*32,1);


data = reshape(bs_network, 32*32, 759);
msk = reshape(t_mask,32*32,1);

msk(find(bbb==0)) = 0; % remove duplicated 

ddd = bbb(find(msk>0)); % for pointing back to specific link

data2 = data(find(msk>0),:);

T = readtable('SubInfo.txt','Delimiter','\t');
age = table2array(T(:,2));
[Y,I] = sort(age); 

data3 = data2(:,I);
%1:159; 160:541; 542:759

data4(:,1) = mean(data3(:,1:159),2);
data4(:,2) = mean(data3(:,160:541),2);
data4(:,3) = mean(data3(:,542:759),2);

figure; imagesc(data4)

% 16, 55: neg, neg, pos
% link 142, link 445
% check aaa: 
[x1,y1] = find(aaa==142); % 14,5: sgACC.R; DMPFC.L
[x2,y2] = find(aaa==445); % 29, 14: ; IPS.R; sgACC.R
% all other links: pos, pos, pos



%---
data = nbs.NBS.test_stat;
data(data<3)=0;
data = triu(data,2);
load('roi_name.mat')

imagesc(data)
xticklabels = roi_name; %roi_name;
xticks = linspace(1, size(data,2), numel(xticklabels));
set(gca, 'XTick', xticks, 'xaxisLocation','top','XTickLabelRotation', 90,'XTickLabel', xticklabels);

load('roi_name.mat')
yticklabels = roi_name;
yticks = linspace(1, size(data,1), numel(yticklabels));
set(gca, 'YTick', yticks, 'YTickLabel', yticklabels(:));
