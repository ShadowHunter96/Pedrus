package cz.bbn.cerberus.areatechnologysign;

import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.areatechnologysign.ui.AreaTechnologySignAddDialog;
import cz.bbn.cerberus.areatechnologysign.ui.AreaTechnologySignsBadgeComponent;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteActionByDto;
import cz.bbn.cerberus.commons.component.ui.interfaces.ListAction;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.listconfiguration.ListService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class AreaTechnologyComponentOperation {

    private final AreaTechnologySignService areaTechnologySignService;
    private final ListService listService;
    private final AppEnv appEnv;

    public AreaTechnologyComponentOperation(AreaTechnologySignService areaTechnologySignService,
                                            ListService listService, AppEnv appEnv) {
        this.areaTechnologySignService = areaTechnologySignService;
        this.listService = listService;
        this.appEnv = appEnv;
    }

    public ComponentEventListener<ClickEvent<Button>> getAreaTechnologySignEvent(ObjectType objectType, String objectId,
                                                                                 AreaTechnologySignsBadgeComponent areaTechnologySignsBadgeComponent) {
        return buttonClickEvent -> {
            AreaTechnologySignAddDialog areaTechnologySignAddDialog = new AreaTechnologySignAddDialog(objectType, objectId, listService,
                    this, appEnv, areaTechnologySignsBadgeComponent);
            areaTechnologySignAddDialog.open();
        };
    }

    public ListAction<AreaTechnologySignDto> getListAction(ObjectType objectType, String objectId){
        return id ->
             areaTechnologySignService.areaTechnologySignDtoList(objectType, objectId);
    }

    public DeleteActionByDto<AreaTechnologySignDto> getDeleteAction(
            AreaTechnologySignsBadgeComponent areaTechnologySignsBadgeComponent){
        return dto -> {
            areaTechnologySignService.deleteAreaTechnologySign(dto.getId());
            areaTechnologySignsBadgeComponent.loadData();
        };
    }

    public List<AreaTechnologySignDto> getAreaTechnologySignDtoList(ObjectType objectType, String objectId){
        return areaTechnologySignService.areaTechnologySignDtoList(objectType, objectId);
    }

    public void saveAreaTechnologySign(AreaTechnologySignDto dto, AreaTechnologySignAddDialog dialog){
        try {
            areaTechnologySignService.saveAreaTechnologySign(dto);
            dialog.close();
            SuccessNotification.showSavingSuccess(appEnv);
        } catch (SystemException e) {
            log.error(TextValues.SYSTEM_EXCEPTION, e);
            ErrorNotification.show(e, appEnv);
        }
    }


}
