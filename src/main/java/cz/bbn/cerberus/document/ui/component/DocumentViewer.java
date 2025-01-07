package cz.bbn.cerberus.document.ui.component;

import com.helger.commons.csv.CSVParser;
import com.vaadin.componentfactory.pdfviewer.PdfViewer;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Image;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.server.StreamResource;
import cz.bbn.cerberus.document.DocumentComponentOperation;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.IOUtils;
import org.vaadin.stefan.table.Table;
import org.vaadin.stefan.table.TableCell;
import org.vaadin.stefan.table.TableRow;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

@Slf4j
public class DocumentViewer extends VerticalLayout {

    private static final String PARSING_ERROR_MESSAGE = "Error parsing text";

    private final DocumentComponentOperation documentComponentOperation;
    private final String documentId;

    public DocumentViewer(String documentId, DocumentComponentOperation documentComponentOperation,
                          DocumentFileTypeEnum documentType) {
        this.documentComponentOperation = documentComponentOperation;
        this.documentId = documentId;
        setMargin(false);
        setPadding(false);

        switch (documentType) {
            case PDF -> showPdf();
            case PICTURE -> showPicture();
            case TXT -> showText();
            case CSV -> showCsv();
            default -> showErrorWindow();
        }
    }
    public DocumentViewer(String name, InputStream inputStream) {
        this.documentComponentOperation = null;
        this.documentId = name;
        setMargin(false);
        setPadding(false);
        showPdf(inputStream);
    }

    private void showPdf(InputStream inputStream) {
        PdfViewer pdfViewer = new PdfViewer();
        StreamResource resource = new StreamResource(documentId, () -> inputStream);
        pdfViewer.setSrc(resource);
        pdfViewer.setAddDownloadButton(false);
        pdfViewer.addClassName("pdf-viewer-height");
        addClassName("pdf-layout-width");
        addClassName("pdf-layout-height");
        setWidth("100%");
        setHeight("100%");
        add(pdfViewer);
    }

    private void showPdf() {
        showPdf(documentComponentOperation.getItemActionDocumentFile().getItem(documentId).getDocumentFileDto().getFileData());
    }

    private void showPicture() {
        Image image = new Image();
        image.setSrc(new StreamResource(documentId, () -> documentComponentOperation
                .getItemActionDocumentFile().getItem(documentId).getDocumentFileDto().getFileData()));
        add(image);
    }

    private void showText() {
        InputStream inputStream = documentComponentOperation.getItemActionDocumentFile()
                .getItem(documentId).getDocumentFileDto().getFileData();
        String text;
        try {
            text = IOUtils.toString(inputStream, StandardCharsets.UTF_8);
        } catch (IOException e) {
            log.error(PARSING_ERROR_MESSAGE, e);
            text = Transl.get(PARSING_ERROR_MESSAGE);
        }

        Div textDiv = new Div();
        textDiv.setText(text);
        textDiv.setSizeFull();
        add(textDiv);
    }

    private void showCsv() {
        InputStream inputStream = documentComponentOperation.getItemActionDocumentFile()
                .getItem(documentId).getDocumentFileDto().getFileData();
        String text;
        CSVParser csvParser = new CSVParser();
        Table table = new Table();
        try {
            text = IOUtils.toString(inputStream, StandardCharsets.UTF_8);

            boolean first = true;
            for (String line : text.lines().toList()) {
                TableRow row = table.addRow();
                List<String> dataArr = csvParser.parseLine(line);
                if (dataArr == null) {
                    dataArr = new ArrayList<>();
                }
                if (first) {
                    for (String cell : dataArr) {
                        TableCell headerCell = row.addHeaderCell();
                        headerCell.setText(cell);
                    }
                    first = false;
                } else {
                    generateNormalCsvRow(dataArr, row);
                }
            }
        } catch (IOException e) {
            log.error(PARSING_ERROR_MESSAGE, e);
            text = Transl.get(PARSING_ERROR_MESSAGE);
            TableRow row = table.addRow();
            TableCell tableCell = row.addDataCell();
            tableCell.setText(text);
        }
        table.setSizeFull();
        add(table);
    }

    private void generateNormalCsvRow(List<String> dataArr, TableRow row) {
        int i = 1;
        for (String cell : dataArr) {
            TableCell normalCell = row.addDataCell();
            normalCell.setText(cell);
            normalCell.getElement().getStyle().set("padding-left", "4px");
            normalCell.getElement().getStyle().set("padding-right", "4px");
            if (i % 2 == 1) {
                normalCell.getElement().getStyle().set("background-color", "rgb(220,220,220, 0.2)");
            }
            i++;
        }
    }

    private void showErrorWindow() {
        Div textDiv = new Div();
        textDiv.setText("Unsupported document type for view");
        textDiv.setSizeFull();
        add(textDiv);
    }
}
