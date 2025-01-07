package cz.bbn.cerberus.area;

import com.vaadin.flow.component.UI;
import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.administration.ui.component.AreaTabComponent;
import cz.bbn.cerberus.area.dto.AreaDto;
import cz.bbn.cerberus.area.dto.AreaFilterDto;
import cz.bbn.cerberus.area.ui.AreaDetailView;
import cz.bbn.cerberus.area.ui.component.AreaFilterComponent;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class AreaComponentOperation  {

    private final AreaService areaService;
    private final AppEnv appEnv;

    public AreaComponentOperation(AreaService areaService, AppEnv appEnv) {
        this.areaService = areaService;
        this.appEnv = appEnv;
    }

    public SaveAction<AreaDto> getSaveAction(AppDialog appDialog) {
        return (newDto, originalDto) -> {
            try {
                if (originalDto.getId() != null) {
                    areaService.updateArea(newDto, originalDto);
                    UI.getCurrent().navigate(AdministrationView.ROUTE.concat("/")
                            .concat(String.valueOf(AreaTabComponent.TAB_INDEX)));
                } else {
                    newDto.setDeleted(Boolean.FALSE);
                    areaService.saveArea(newDto);
                    if (appDialog != null) {
                        appDialog.showWarning(false);
                        appDialog.close();
                        UI.getCurrent().navigate(AreaDetailView.ROUTE.concat("/").concat(newDto.getId()));
                    }
                }
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    public List<AreaDto> findAllAreaDtoList() {
        return areaService.findAllNotDeletedAreaDtoList();
    }

    public ItemsAction<AreaDto> getItemsAction(AreaFilterComponent areaFilterComponent) {
        return (query, orderList) -> {
            AreaFilterDto filter = areaFilterComponent.getAreaFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return areaService.findAreaDtoPage(filter);
        };
    }

}
