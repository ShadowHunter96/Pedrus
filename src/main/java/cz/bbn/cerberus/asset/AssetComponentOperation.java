package cz.bbn.cerberus.asset;

import com.vaadin.flow.component.UI;
import cz.bbn.cerberus.asset.dto.AssetByObjectDto;
import cz.bbn.cerberus.asset.dto.AssetByObjectFilterDto;
import cz.bbn.cerberus.asset.dto.AssetDto;
import cz.bbn.cerberus.asset.dto.AssetFilterDto;
import cz.bbn.cerberus.asset.dto.AssetSimpleDto;
import cz.bbn.cerberus.asset.ui.AssetDetailView;
import cz.bbn.cerberus.asset.ui.AssetView;
import cz.bbn.cerberus.asset.ui.component.AssetFilterComponent;
import cz.bbn.cerberus.asset.ui.component.AssetNewDialog;
import cz.bbn.cerberus.assetposition.AssetPositionService;
import cz.bbn.cerberus.assetposition.dto.AssetPositionDto;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.UnlinkAction;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.employee.EmployeeService;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

@Component
@Slf4j
public class AssetComponentOperation {

    private final AssetService assetService;
    private final AssetPositionService assetPositionService;
    private final ListService listService;
    private final EmployeeService employeeService;
    private final AppEnv appEnv;

    public AssetComponentOperation(AssetService assetService, AssetPositionService assetPositionService,
                                   ListService listService, EmployeeService employeeService, AppEnv appEnv) {
        this.assetService = assetService;
        this.assetPositionService = assetPositionService;
        this.listService = listService;
        this.employeeService = employeeService;
        this.appEnv = appEnv;
    }

    public SaveAction<AssetDto> getSaveAction(AssetNewDialog dialog) {
        return (dto, originalDto) -> {
            try {
                if (assetService.assetExists(dto.getId())) {
                    assetService.updateAsset(dto, originalDto);
                    UI.getCurrent().navigate(AssetView.ROUTE);
                } else {
                    assetService.saveAsset(dto);
                    if (dialog != null) {
                        dialog.showWarning(false);
                        dialog.close();
                    }
                    UI.getCurrent().navigate(AssetDetailView.ROUTE.concat("/").concat(dto.getId()));
                }
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    public List<AssetPositionDto> getAssetPositionDtoList() {
        return assetPositionService.getAssetPositionDtoList();
    }

    public ItemsAction<AssetSimpleDto> getItemsAction(AssetFilterComponent assetFilterComponent) {
        return (query, orderList) -> {
            AssetFilterDto filter = assetFilterComponent.getAssetFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return assetService.findAssetPage(filter);
        };
    }

    public DeleteAction getDeleteAction() {
        return id -> {
            try {
                assetService.delete(id);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    public List<SubjectDto> getSubjectList() {
        List<SubjectDto> ourCompanyList = new ArrayList<>();
        List<SubjectDto> subjectList = listService.getSubjectDtoList();
        for (SubjectDto subjectDto : subjectList) {
            if (!Boolean.TRUE.equals(subjectDto.getDeleted()) && Boolean.TRUE.equals(subjectDto.getOwnCompany())) {
                ourCompanyList.add(subjectDto);
            }
        }
        return ourCompanyList;
    }

    public List<EnumerationDto> getAssetTypeList() {
        List<EnumerationDto> allowedAssetType = new ArrayList<>();
        List<EnumerationDto> allAssetType = listService.getEnumerationDtoList("ASSET_TYPE");
        for (EnumerationDto enumerationDto : allAssetType) {
            if (Boolean.TRUE.equals(enumerationDto.getAllowed())) {
                allowedAssetType.add(enumerationDto);
            }
        }
        return allowedAssetType;
    }

    public void linkAsset(Set<AssetSimpleDto> selectedItems, String objectId, ObjectType objectType) {
        assetService.linkAsset(selectedItems, objectId, objectType);
    }

    public ItemsAction<AssetByObjectDto> getConnectedItemsAction(String objectId, ObjectType objectType) {
        return (query, orderList) -> {
            AssetByObjectFilterDto filter = new AssetByObjectFilterDto();
            filter.setObjectId(objectId);
            filter.setObjectType(objectType);
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return assetService.findConnectedAssetPage(filter);
        };
    }

    public UnlinkAction getUnlinkAction() {
        return id -> {
            try {
                assetService.unlinkItem(id);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    public ItemsAction<AssetSimpleDto> getLinkItemsAction(AssetFilterComponent assetFilterComponent, String id) {
        return (query, orderList) -> {
            AssetFilterDto filter = assetFilterComponent.getAssetFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return assetService.findAssetExceptOnePage(filter, id);
        };
    }

    public String getEmployeeName(String employeeId) {
        return employeeService.getEmployeeName(employeeId);
    }

    public String getEmployeeName(EmployeeDto employeeDto) {
        return employeeService.getEmployeeName(employeeDto);
    }
}
