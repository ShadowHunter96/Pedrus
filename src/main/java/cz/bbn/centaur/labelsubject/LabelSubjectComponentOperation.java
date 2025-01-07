package cz.bbn.cerberus.labelsubject;

import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.label.ui.component.LabelLinkDialog;
import cz.bbn.cerberus.labelsubject.dto.LabelSubjectDto;
import cz.bbn.cerberus.labelsubject.dto.LabelSubjectFilterDto;
import cz.bbn.cerberus.labelsubject.ui.component.LabelSubjectExitsDialog;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class LabelSubjectComponentOperation {

    private final LabelSubjectService labelSubjectService;
    private final AppEnv appEnv;

    public LabelSubjectComponentOperation(LabelSubjectService labelSubjectService, AppEnv appEnv) {
        this.labelSubjectService = labelSubjectService;
        this.appEnv = appEnv;
    }

    public ItemsAction<LabelSubjectDto> getItemsAction(String subjectId) {
        return (query, orderList) -> {
            LabelSubjectFilterDto filter = new LabelSubjectFilterDto();
            filter.setSubjectId(subjectId);
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return labelSubjectService.findLabelSubjectDtoPage(filter);
        };
    }

    public SaveAction<LabelSubjectDto> getSaveAction(LabelLinkDialog dialog, AppInfiniteGrid<LabelSubjectDto> grid,
                                                     boolean checkLabelExits) {
        return (dto, originalDto) -> {
            if (labelSubjectService.subjectLabelExist(dto.getSubjectId(), dto.getLabelDto().getId()) && checkLabelExits) {
                LabelSubjectExitsDialog labelSubjectExitsDialog = new LabelSubjectExitsDialog(this, dto, grid, dialog);
                labelSubjectExitsDialog.open();
                return;
            }
            labelSubjectService.saveLabelSubject(dto);
            dialog.close();
            grid.loadData();
            SuccessNotification.showSavingSuccess(appEnv);
        };
    }

    public DeleteAction getDeleteAction(String subjectId) {
        return id -> {
            try {
                labelSubjectService.deleteLabelSubject(Long.valueOf(id), subjectId);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }
}
