package cz.bbn.cerberus.enumeration;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.combobox.ComboBox;
import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationFilterDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationTypeDto;
import cz.bbn.cerberus.enumeration.ui.EnumerationDetailView;
import cz.bbn.cerberus.enumeration.ui.component.EnumerationFilterComponent;
import cz.bbn.cerberus.enumeration.ui.component.EnumerationTabComponent;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
@Slf4j
public class EnumerationComponentOperation {

    private final EnumerationService enumerationService;
    private final AppEnv appEnv;
    private final ListService listService;

    public EnumerationComponentOperation(EnumerationService enumerationService, AppEnv appEnv,
                                         ListService listService) {
        this.enumerationService = enumerationService;
        this.appEnv = appEnv;
        this.listService = listService;
    }

    public ComboBox<EnumerationDto> getEnumerationCombobox(String enumerationTypeId) {
        return getEnumerationCombobox(null, enumerationTypeId);
    }

    public ComboBox<EnumerationDto> getEnumerationCombobox(String title, String enumerationTypeId) {
        List<EnumerationDto> enumerationDtoList = listService.getEnumerationDtoList(enumerationTypeId);
        String translationKey = enumerationDtoList.isEmpty() ? "" :
                enumerationDtoList.get(0).getEnumerationTypeDto().getTranslationKey();
        ComboBox<EnumerationDto> enumeration = new ComboBox<>(title == null ? Transl.get(translationKey) : title);
        enumeration.setItemLabelGenerator(EnumerationDto::getName);
        enumeration.setItems(enumerationService.getEnumerationDtoListByTypeNotDeletedAllowed(enumerationTypeId));
        return enumeration;
    }

    public List<TabEntry> generateEnumerationTabs(int startTabIndex) {
        List<TabEntry> tabEntryList = new ArrayList<>();
        List<EnumerationTypeDto> enumerationTypeDtoList = getEnumerationTypeDtoList();
        for (EnumerationTypeDto enumerationTypeDto : enumerationTypeDtoList) {
            if (Permission.valueOfOrNotExists(enumerationTypeDto.getPermissionKey().concat("_VIEW"))
                    != Permission.NON_EXISTENT_PERMISSION
                    && SecurityUtils.hasPermission(
                    Permission.valueOf(enumerationTypeDto.getPermissionKey().concat("_VIEW"))
            )) {
                TabEntry tabEntry = new TabEntry(
                        Transl.get(enumerationTypeDto.getTranslationKey().concat(" list")),
                        new EnumerationTabComponent(startTabIndex, appEnv, this, enumerationTypeDto),
                        Permission.valueOf(enumerationTypeDto.getPermissionKey().concat("_VIEW")),
                        startTabIndex++);
                tabEntryList.add(tabEntry);
            }
        }
        return tabEntryList;
    }

    public SaveAction<EnumerationDto> getSaveAction(AppDialog appDialog, Integer tabIndex) {
        return (newDto, originalDto) -> {
            try {
                if (originalDto != null) {
                    enumerationService.updateEnumeration(newDto, originalDto);
                    UI.getCurrent().navigate(AdministrationView.ROUTE.concat("/").concat(String.valueOf(tabIndex)));
                } else {
                    newDto.setDeleted(Boolean.FALSE);
                    Long id = enumerationService.saveEnumeration(newDto);
                    if (appDialog != null) {
                        appDialog.showWarning(false);
                        appDialog.close();
                        UI.getCurrent().navigate(
                                EnumerationDetailView.ROUTE.concat("/")
                                        .concat(String.valueOf(id))
                                        .concat("-")
                                        .concat(String.valueOf(tabIndex)));
                    }
                }
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    public ItemsAction<EnumerationDto> getItemsAction(EnumerationFilterComponent enumerationFilterComponent,
                                                      String enumerationTypeId) {
        return (query, orderList) -> {
            EnumerationFilterDto filter = enumerationFilterComponent.getEnumerationFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            filter.setEnumerationTypeId(enumerationTypeId);
            return enumerationService.findEnumerationDtoPage(filter);
        };
    }

    public DeleteAction getDeleteAction() {
        return id -> {
            try {
                enumerationService.deleteEnumeration(Long.valueOf(id));
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    public List<EnumerationDto> getByDefaultValueTrueList(Long id, String enumerationTypeId) {
        return enumerationService.getDefaultValueTrueList(id, enumerationTypeId);
    }

    public List<EnumerationTypeDto> getEnumerationTypeDtoList() {
        return enumerationService.getEnumerationTypeDtoList();
    }
}
