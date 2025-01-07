package cz.bbn.cerberus.activity;

import cz.bbn.cerberus.activity.dto.ActivityByObjectDto;
import cz.bbn.cerberus.activity.dto.ActivityByObjectFilterDto;
import cz.bbn.cerberus.activity.dto.ActivityLinkDto;
import cz.bbn.cerberus.activity.persistance.ActivityByObjectId;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class ActivityByObjectComponentOperation {

    private final ActivityByObjectService activityByObjectService;
    private final AppEnv appEnv;

    public ActivityByObjectComponentOperation(ActivityByObjectService activityByObjectService, AppEnv appEnv) {
        this.activityByObjectService = activityByObjectService;
        this.appEnv = appEnv;
    }

    public ItemsAction<ActivityByObjectDto> getItemsAction(String objectId, ObjectType objectType) {
        return (query, orderList) -> {
            ActivityByObjectFilterDto filter = new ActivityByObjectFilterDto();
            filter.setObjectId(objectId);
            filter.setObjectType(objectType);
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return activityByObjectService.findActivityByObjectEntityPage(filter);
        };
    }

    public List<Long> getLinkedActivityEmployeeList(String objectId, ObjectType objectType){
        return activityByObjectService.getLinkedActivityIdList(objectId, objectType);
    }

    public SaveAction<ActivityLinkDto> getSaveAction() {
        return (dto, originalDto) -> {
            activityByObjectService.saveActivityByObject(dto);
            SuccessNotification.showSavingSuccess(appEnv);
        };
    }

    public DeleteAction getDeleteAction(String objectId, ObjectType objectType) {
        return id -> {
            try {
                activityByObjectService.deleteActivityByObject(new ActivityByObjectId(Long.valueOf(id),
                        objectId, objectType));
                SuccessNotification.showDeleteSuccess(appEnv);
            } catch (SystemException e) {
                log.error("delete activity by object error", e);
                ErrorNotification.show(e, appEnv);
            }
        };
    }
}
