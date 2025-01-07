package cz.bbn.cerberus.technology;

import com.vaadin.flow.component.UI;
import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.administration.ui.component.TechnologyTabComponent;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.technology.dto.TechnologyDto;
import cz.bbn.cerberus.technology.dto.TechnologyFilterDto;
import cz.bbn.cerberus.technology.ui.TechnologyDetailView;
import cz.bbn.cerberus.technology.ui.component.TechnologyFilterComponent;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class TechnologyComponentOperation {

    private final TechnologyService technologyService;
    private final AppEnv appEnv;

    public TechnologyComponentOperation(TechnologyService technologyService, AppEnv appEnv) {
        this.technologyService = technologyService;
        this.appEnv = appEnv;
    }

    public SaveAction<TechnologyDto> getSaveAction(AppDialog appDialog) {
        return (newDto, originalDto) -> {
            try {
                if (originalDto.getId() != null) {
                    technologyService.updateTechnology(newDto, originalDto);
                    UI.getCurrent().navigate(AdministrationView.ROUTE.concat("/")
                            .concat(String.valueOf(TechnologyTabComponent.TAB_INDEX)));
                } else {
                    newDto.setDeleted(Boolean.FALSE);
                    technologyService.saveTechnology(newDto);
                    if (appDialog != null) {
                        appDialog.showWarning(false);
                        appDialog.close();
                        UI.getCurrent().navigate(TechnologyDetailView.ROUTE.concat("/").concat(newDto.getId()));
                    }
                }
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    public ItemsAction<TechnologyDto> getItemsAction(TechnologyFilterComponent areaFilterComponent) {
        return (query, orderList) -> {
            TechnologyFilterDto filter = areaFilterComponent.getTechnologyFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return technologyService.findTechnologyDtoPage(filter);
        };
    }

    public List<TechnologyDto> findAllowedTechnologyList() {
        return technologyService.findAllowedTechnologyDtoList();
    }
}
