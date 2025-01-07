package cz.bbn.cerberus.offer;

import com.vaadin.flow.component.UI;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.offer.dto.OfferDto;
import cz.bbn.cerberus.offer.dto.OfferFilterDto;
import cz.bbn.cerberus.offer.ui.OfferDetailView;
import cz.bbn.cerberus.offer.ui.component.OfferFilterComponent;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.opportunity.ui.OpportunityDetailView;
import cz.bbn.cerberus.task.TaskComponentOperation;
import cz.bbn.cerberus.task.dto.TaskDto;
import cz.bbn.cerberus.task.dto.TaskEntityType;
import cz.bbn.cerberus.task.ui.component.TaskEditDialog;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

@Component
@Slf4j
public class OfferComponentOpperation {

    private final OfferService offerService;
    private final AppEnv appEnv;
    private final TaskComponentOperation taskComponentOperation;
    private final ListService listService;

    public OfferComponentOpperation(OfferService offerService, AppEnv appEnv,
                                    TaskComponentOperation taskComponentOperation,
                                    ListService listService) {
        this.offerService = offerService;
        this.appEnv = appEnv;
        this.taskComponentOperation = taskComponentOperation;
        this.listService = listService;
    }

    public ItemsAction<OfferDto> getItemsAction(OfferFilterComponent filterComponent) {
        return (query, orderList) -> {
            OfferFilterDto filter = filterComponent.getOfferFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return offerService.findOfferDtoPage(filter);
        };
    }

    public SaveAction<OfferDto> getSaveAction(AppDialog appDialog) {
        return (newDto, originalDto) -> {
            try {
                if (originalDto.getId() != null) {
                    offerService.updateOffer(newDto, originalDto);
                    UI.getCurrent().navigate(OpportunityDetailView.ROUTE.concat("/")
                            .concat(originalDto.getOpportunityDto().getId().replace("/", "&ndash")));
                    if (newDto.getState() != originalDto.getState()) {
                        createEvent(newDto);
                    }
                } else {
                    offerService.saveOffer(newDto);
                    if (appDialog != null) {
                        appDialog.showWarning(false);
                        appDialog.close();
                        UI.getCurrent().navigate(OfferDetailView.ROUTE.concat("/")
                                .concat(newDto.getId().replace("/", "&ndash")));
                    }
                }
                SuccessNotification.showSavingSuccess(appEnv);

            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    private void createEvent(OfferDto offerDto) {
        TaskDto taskDto = new TaskDto();
        taskDto.setUserDto(offerDto.getProcessedByUserDto());
        taskDto.setName(offerDto.getName());
        taskDto.setUserDtoSet(new HashSet<>());

        TaskEditDialog taskEditDialog = new TaskEditDialog(
                taskDto, null, taskComponentOperation, null, TaskEntityType.TASK);
        taskEditDialog.open();
    }

    public DeleteAction getDeleteAction() {
        return id -> {
            try {
                offerService.delete(id);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    public List<OpportunityDto> findBySubjectId(String id) {
        List<OpportunityDto> opportunityDtoList = new ArrayList<>();
        for (OpportunityDto opportunityDto : listService.getOpportunityDtoList()) {
            if (!Boolean.TRUE.equals(opportunityDto.getDeleted()) && opportunityDto.getSubject() != null
                    && opportunityDto.getSubject().getId().equals(id)) {
                opportunityDtoList.add(opportunityDto);
            }
        }
        return opportunityDtoList;
    }
}
