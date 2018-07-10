fid   = fopen('subjs_PNC_759.txt');
sub = textscan(fid, '%s');
fclose(fid);

sub = sub{1,1};
con = {'neutral','fear','angry','sad','happy','threat','all','salient'};

for j= 1:8 % 2 == fear condition
    for i=1:length(sub)
        fname = sprintf('Individuals/BetaCorr_network_EID55_%s.mat',sub{i,1});
        load(fname); % roi_names, C, P; 
        % C:1*5 cell, including neutral, fear, angry, sad, and happy
        % each cell of C is 32*32 matrix

        if i==1
            bs_network = C{1,j}; 
        else
            bs_network = cat(3,bs_network,C{1,j}); 
        end
    end
    
    output = sprintf('bs_network_%s_759s.mat',con{j});
    save(output,'bs_network','roi_names');
end


% T = readtable('SubInfo.txt','Delimiter','\t');
% T = table2array(T(:,[2 3 4 5 6 8]));
% % T = T(find(T(:,5)==0),:); % 0 means nongad group; 1 means gad group
% % T = T(:,[1 2 3 4 6]); % age, gscore, sex, race, motion
% 
% load('bs_network_salient_759s.mat')
% data = bs_network; % 32*32*759
% nong = data(:,:,find(T(:,5)==0));
% gad = data(:,:,find(T(:,5)==1));
% matrix = cat(3,nong,gad);
% 
% save('bs_network_salient_759s_NBS.mat','matrix')

% %---check data---
% neutral = load('bs_network_neutral_759s.mat');
% fear = load('bs_network_fear_759s.mat');
% angry = load('bs_network_angry_759s.mat');
% sad = load('bs_network_sad_759s.mat');
% happy = load('bs_network_happy_759s.mat');
% 
% nn = nanmean(neutral.bs_network,3);
% ff = nanmean(fear.bs_network,3);
% aa = nanmean(angry.bs_network,3);
% ss = nanmean(sad.bs_network,3);
% hh = nanmean(happy.bs_network,3);
% 
% roiname = readtable('ROI_name.txt','Delimiter','\t');
% roiname = table2cell(roiname);
% 
% 
% dd = aa;
% figure; 
% imagesc(dd)
% xticklabels = roiname;
% xticks = linspace(1, size(dd,2), numel(xticklabels));
% %set(gca, 'XTick', xticks, 'XTickLabel', xticklabels);
% set(gca, 'XTick', xticks, 'xaxisLocation','top','XTickLabelRotation', 90,'XTickLabel', xticklabels);
% 
% yticklabels = roiname;
% yticks = linspace(1, size(dd,1), numel(yticklabels));
% set(gca, 'YTick', yticks, 'YTickLabel', yticklabels(:));
