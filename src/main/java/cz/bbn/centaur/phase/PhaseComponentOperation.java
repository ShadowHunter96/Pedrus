package cz.bbn.cerberus.phase;

import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.phase.dto.PhaseDto;
import cz.bbn.cerberus.phase.dto.PhaseFilterDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class PhaseComponentOperation {

    private final PhaseService phaseService;
    private final AppEnv appEnv;

    public PhaseComponentOperation(PhaseService phaseService, AppEnv appEnv) {
        this.phaseService = phaseService;
        this.appEnv = appEnv;
    }

    public SaveAction<PhaseDto> getSaveAction() {
        return (newDto, originalDto) -> {
            try {
                if (originalDto.getId() != null) {
                    phaseService.updatePhase(newDto, originalDto);
                } else {
                    phaseService.savePhase(newDto);
                }
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    public ItemsAction<PhaseDto> getItemsAction(String projectId) {
        return (query, orderList) -> {
            PhaseFilterDto filter = new PhaseFilterDto();
            filter.setProjectId(projectId);
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return phaseService.findPhaseEntityPage(filter);
        };
    }
}
