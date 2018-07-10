T = readtable('SubInfo.txt','Delimiter','\t');
age = table2array(T(:,2));
agebin = table2cell(T(:,7));
roi = readtable('roiname_list.txt','Delimiter','\t','ReadVariableNames',false);
roi_name = table2cell(roi);

load('bs_network_fear_759s.mat')
CH = nanmean(bs_network(:,:,find(strcmp(agebin,'Children'))),3);
ADO = nanmean(bs_network(:,:,find(strcmp(agebin,'Adolescence'))),3);
ADU = nanmean(bs_network(:,:,find(strcmp(agebin,'Adult'))),3);

save('CH_bs_cor_mean.mat','CH')
save('ADO_bs_cor_mean.mat','ADO')
save('ADU_bs_cor_mean.mat','ADU')


load('lm results to network/New Strategy/fear_age_coef.mat')
coef_mat(coef_mat>0 | coef_mat<0) = 1;
mask = coef_mat; 

%--plot
diff1 = ADO-CH;
diff2 = ADU-ADO;

CH(mask==0) = 0
diff1(mask==0) = 0;
diff2(mask==0) = 0;

lim = max(abs(min(CH(:))),max(CH(:)));
imagesc(CH);
caxis([-lim lim])
caxis([-1 1])
xticklabels = roi_name; %(I); %roi_name;
xticks = linspace(1, size(CH,2), numel(xticklabels));
set(gca, 'XTick', xticks, 'xaxisLocation','bottom','XTickLabelRotation', 45,'XTickLabel', xticklabels);

yticklabels = roi_name; %(I);
yticks = linspace(1, size(CH,1), numel(yticklabels));
set(gca, 'YTick', yticks, 'YTickLabel', yticklabels(:));


lim = max(abs(min(diff1(:))),max(diff1(:)));
figure;
imagesc(diff1);
caxis([-lim lim])
%caxis([-2 2])
xticklabels = roi_name; %(I); %roi_name;
xticks = linspace(1, size(ADO,2), numel(xticklabels));
set(gca, 'XTick', xticks, 'xaxisLocation','bottom','XTickLabelRotation', 45,'XTickLabel', xticklabels);

yticklabels = roi_name; %(I);
yticks = linspace(1, size(ADO,1), numel(yticklabels));
set(gca, 'YTick', yticks, 'YTickLabel', yticklabels(:));

lim = max(abs(min(diff2(:))),max(diff2(:)));
figure;
imagesc(diff2);
caxis([-lim lim])
%caxis([-2 2])
xticklabels = roi_name; %(I); %roi_name;
xticks = linspace(1, size(ADO,2), numel(xticklabels));
set(gca, 'XTick', xticks, 'xaxisLocation','bottom','XTickLabelRotation', 45,'XTickLabel', xticklabels);

yticklabels = roi_name; %(I);
yticks = linspace(1, size(ADO,1), numel(yticklabels));
set(gca, 'YTick', yticks, 'YTickLabel', yticklabels(:));
