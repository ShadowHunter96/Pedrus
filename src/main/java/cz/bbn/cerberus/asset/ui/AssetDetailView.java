package cz.bbn.cerberus.asset.ui;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.asset.AssetComponentOperation;
import cz.bbn.cerberus.asset.AssetService;
import cz.bbn.cerberus.asset.dto.AssetDto;
import cz.bbn.cerberus.asset.ui.component.AssetFilterComponent;
import cz.bbn.cerberus.asset.ui.component.AssetLinkDialog;
import cz.bbn.cerberus.asset.ui.component.AssetTabsComponent;
import cz.bbn.cerberus.asset.ui.component.tab.AssetConnectedTab;
import cz.bbn.cerberus.asset.ui.component.tab.AssetDetailTab;
import cz.bbn.cerberus.asset.ui.component.tab.AssetDocumentTab;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.slidetab.CountIntIndicator;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.document.DocumentComponentOperation;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteComponentOperation;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.note.ui.component.NoteComponent;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.task.TaskComponentOperation;
import cz.bbn.cerberus.task.ui.component.TaskSlideTabComponent;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;

@Route(value = AssetDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.ASSET_VIEW)
@Slf4j
public class AssetDetailView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "asset-detail";

    private final AppEnv appEnv;
    private final AssetService assetService;
    private final AssetComponentOperation assetComponentOperation;
    private final DocumentComponentOperation documentComponentOperation;
    private final ListService listService;
    private final TaskComponentOperation taskComponentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final NoteComponentOperation noteComponentOperation;

    private AssetDto dto;
    private boolean readOnly = true;

    public AssetDetailView(AppEnv appEnv, AssetService assetService, AssetComponentOperation assetComponentOperation,
                           DocumentComponentOperation documentComponentOperation, ListService listService,
                           TaskComponentOperation taskComponentOperation,
                           EntityNewComponentOperation entityNewComponentOperation,
                           NoteComponentOperation noteComponentOperation) {
        this.appEnv = appEnv;
        this.assetService = assetService;
        this.assetComponentOperation = assetComponentOperation;
        this.documentComponentOperation = documentComponentOperation;
        this.listService = listService;
        this.taskComponentOperation = taskComponentOperation;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.noteComponentOperation = noteComponentOperation;
    }

    private void initView() {
        removeAll();
        List<TabEntry> tabEntryList = new ArrayList<>();

        String heading = dto.getId() == null ? Transl.get("New asset") :
                Transl.get("Asset")
                        .concat(" - ")
                        .concat(dto.getName());

        tabEntryList.add(new TabEntry(Transl.get("Asset detail"),
                new AssetDetailTab(dto, assetComponentOperation.getSaveAction(null), appEnv,
                        assetComponentOperation.getAssetPositionDtoList(), false, readOnly,
                        assetComponentOperation.getSubjectList(), listService.getEmployeeDtoList(),
                        assetComponentOperation.getAssetTypeList(), assetComponentOperation)
        ));

        AssetConnectedTab assetConnectedTab = new AssetConnectedTab(
                appEnv, assetComponentOperation.getConnectedItemsAction(dto.getId(), ObjectType.ASSET),
                assetComponentOperation, assetComponentOperation.getUnlinkAction());

        tabEntryList.add(new TabEntry(Transl.get("Linked assets"), assetConnectedTab));

        Button addDocument = VaadinComponents.getNewButton(Transl.get("Add document"), false);
        if (SecurityUtils.hasPermission(Permission.DOCUMENT_VIEW)) {
            tabEntryList.add(new TabEntry(Transl.get("Documents"),
                    new AssetDocumentTab(documentComponentOperation, dto, !readOnly, appEnv, addDocument, listService),
                    Permission.CONTRACT_DOCUMENT_VIEW));
        }

        Button linkAsset = VaadinComponents.getLinkButton(Transl.get("Link asset"));
        linkAsset.addClickListener(e -> {
            Button assetSearchButton = VaadinComponents.getSearchButton();
            AssetFilterComponent filter = new AssetFilterComponent(assetSearchButton);
            String assetId = dto.getId() != null ? dto.getId() : "";
            AssetLinkDialog linkDialog = new AssetLinkDialog(Transl.get("Link asset"), assetComponentOperation,
                    appEnv, assetId, ObjectType.ASSET, assetConnectedTab.getGrid(), filter);
            assetSearchButton.addClickListener(event -> linkDialog.loadData());
            linkDialog.open();
        });

        TaskSlideTabComponent taskSlideTabComponent = new TaskSlideTabComponent(taskComponentOperation, appEnv,
                ObjectType.ASSET, dto.getId(), dto.getOurCompany() != null ? dto.getOurCompany() : null, listService);

        AssetTabsComponent assetTabsComponent = new AssetTabsComponent(
                heading, tabEntryList, SecurityUtils.hasPermission(Permission.ASSET_EDIT) && !readOnly,
                linkAsset, addDocument, entityNewComponentOperation);

        assetTabsComponent.addEventSlideTab(taskSlideTabComponent);

        assetTabsComponent.addNewEntitySlideTab(
                new NewEntityButtonsComponent.Builder(entityNewComponentOperation, EntityNewType.BACKOFFICE)
                        .setSubjectDto(dto.getOurCompany())
                        .build());

        if (SecurityUtils.hasPermission(Permission.ASSET_NOTE_VIEW)) {
            Checkbox showArchived = new Checkbox(Transl.get("Show also archived"));
            CountIntIndicator noteIndicator = new CountIntIndicator(
                    noteComponentOperation.getNoteCountByTypeAndObjectId(NoteTypeEnum.CONTACT_PERSON, dto.getId()));
            NoteComponent noteComponent = new NoteComponent(noteComponentOperation, showArchived,
                    dto.getId(), appEnv, NoteTypeEnum.CONTACT_PERSON, Permission.CONTACT_PERSON_NOTE_VIEW,
                    Permission.CONTACT_PERSON_NOTE_EDIT,
                    true, !readOnly, noteIndicator);
            assetTabsComponent.addNoteSlideTab(noteComponent, noteIndicator);
        }
        add(assetTabsComponent);

    }

    private void setDto(String param) throws SystemException {
        if (assetService.assetExists(param)) {
            dto = assetService.getAsset(param);
            refreshBreadcrumbText(dto.getId());
            if (SecurityUtils.hasPermission(Permission.ASSET_EDIT)) {
                readOnly = false;
            }
            initView();
        } else {
            ErrorNotification.show(ErrorCode.VIEW_PERMISSION_MISSING.getError(), appEnv);
            UI.getCurrent().access(
                    () -> UI.getCurrent().getPage().fetchCurrentURL(e -> UI.getCurrent().navigate(e.getPath()))
            );
        }
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String param) {
        if (param != null) {
            try {
                setDto(param);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        }
    }
}
