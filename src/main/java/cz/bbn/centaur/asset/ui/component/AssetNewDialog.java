package cz.bbn.cerberus.asset.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.asset.AssetComponentOperation;
import cz.bbn.cerberus.asset.dto.AssetDto;
import cz.bbn.cerberus.asset.dto.AssetSimpleDto;
import cz.bbn.cerberus.asset.ui.component.tab.AssetDetailTab;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;

public class AssetNewDialog extends AppDialog {

    private final AppInfiniteGrid<AssetSimpleDto> grid;
    private final AssetComponentOperation assetComponentOperation;
    private final AppEnv appEnv;
    private final ListService listService;
    private final SubjectDto subjectDto;

    public AssetNewDialog(AppInfiniteGrid<AssetSimpleDto> grid, AssetComponentOperation assetComponentOperation,
                          AppEnv appEnv, ListService listService, SubjectDto subjectDto) {
        this.grid = grid;
        this.appEnv = appEnv;
        this.assetComponentOperation = assetComponentOperation;
        this.listService = listService;
        this.subjectDto = subjectDto;
        init();
    }

    void init() {
        setTitle(Transl.get("Add asset"));

        AssetDto dto = new AssetDto();
        dto.setOurCompany(subjectDto);

        Button submit = VaadinComponents.getSubmitButton();

        AssetDetailTab assetDetailTab = new AssetDetailTab(
                dto, assetComponentOperation.getSaveAction(this), appEnv,
                assetComponentOperation.getAssetPositionDtoList(), true, false,
                assetComponentOperation.getSubjectList(), listService.getEmployeeDtoList(),
                assetComponentOperation.getAssetTypeList(), assetComponentOperation);
        setContent(assetDetailTab);

        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            assetDetailTab.saveItem();
            if (grid != null) {
                grid.loadData();
            }
            submit.setEnabled(true);
        });

        showWarning(true);

        addCloseButton();
        addButtons(submit);
    }
}
