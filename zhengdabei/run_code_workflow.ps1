[Console]::OutputEncoding = [System.Text.Encoding]::UTF8
$OutputEncoding = [System.Text.Encoding]::UTF8
if ($PSVersionTable.PSVersion.Major -ge 7) { $PSDefaultParameterValues['*:Encoding'] = 'utf8' }

# ==============================================
# Configuration
# ==============================================
$Color_Script      = "Green"
$Color_Timer       = "Cyan"
$Color_Header      = "DarkCyan"
$Color_File        = "White"
$Color_Warning     = "DarkRed"
$Color_Comment     = "Gray"

$rootPath          = $PWD.Path
$divider           = "--------------------------------------------------------------------------------------"
$exitKey           = [ConsoleKey]::Q
$scripts           = @(
    "code/0.r",
    "code/onehot.r",
    "code/desc.r",
    "code/CA_K-means_plots.r",
    "code/CA_K-means_tables.r",
    "code/Descriptive_stat_C.r",
    "code/Descriptive_stat_E.r",
    "code/Multi_select_corr.r"
)

$runResults        = @()
$totalStartTime    = Get-Date

# ==============================================
# Execute R Scripts
# ==============================================
foreach ($script in $scripts) {
    Write-Host "`n$divider" -ForegroundColor $Color_Header
    Write-Host "Executing : $script" -ForegroundColor $Color_Script
    Write-Host "$divider`n" -ForegroundColor $Color_Header

    $beforeFiles = Get-ChildItem -Path $rootPath -Recurse | Select-Object FullName, LastWriteTime
    Rscript $script
    $currentElapsed = (Get-Date) - $totalStartTime
    $elapsedString  = $currentElapsed.ToString("hh\:mm\:ss\.fff")

    Write-Host "Completed   | Total Elapsed : $elapsedString" -ForegroundColor $Color_Timer

    $afterFiles = Get-ChildItem -Path $rootPath -Recurse | Select-Object FullName, LastWriteTime
    $newFiles = @()

    foreach ($file in $afterFiles) {
        $oldFile = $beforeFiles | Where-Object { $_.FullName -eq $file.FullName }
        if (-not $oldFile -or $file.LastWriteTime -gt $oldFile.LastWriteTime) {
            $newFiles += $file.FullName
        }
    }

    $runResults += [PSCustomObject]@{
        Script          = $script
        TotalElapsed    = $elapsedString
        OutputFiles     = if ($newFiles.Count -gt 0) { $newFiles } else { @("No new files generated") }
    }
}

# ==============================================
# Run Summary
# ==============================================
Write-Host "`n`n$divider" -ForegroundColor $Color_Header
Write-Host "RUN SUMMARY - ALL SCRIPTS" -ForegroundColor $Color_Header
Write-Host "$divider`n" -ForegroundColor $Color_Header

foreach ($res in $runResults) {
    Write-Host "Script       : $($res.Script)" -ForegroundColor $Color_Script
    Write-Host "Total Elapsed: $($res.TotalElapsed)" -ForegroundColor $Color_Timer
    Write-Host "Output       :" -ForegroundColor $Color_Comment
    foreach ($file in $res.OutputFiles) {
        Write-Host "               - $file" -ForegroundColor $Color_File
    }
    Write-Host "`n$divider`n" -ForegroundColor $Color_Comment
}

# ==============================================
# PDF Cleanup
# ==============================================
Write-Host "$divider" -ForegroundColor $Color_Warning
Write-Host "PDF CLEANUP (LAST 30 SECONDS)" -ForegroundColor $Color_Warning
Write-Host "$divider`n" -ForegroundColor $Color_Warning

$pdfFiles = Get-ChildItem "$rootPath" -Filter *.pdf -Recurse | 
            Where-Object { $_.LastWriteTime -gt (Get-Date).AddSeconds(-30) }

if ($pdfFiles) {
    foreach ($file in $pdfFiles) {
        Write-Host "Deleted      : $($file.FullName)" -ForegroundColor $Color_File
        Remove-Item $file.FullName -Force
    }
} else {
    Write-Host "No PDF files to clean up" -ForegroundColor $Color_Comment
}

# ==============================================
# Exit
# ==============================================
Write-Host "`n$divider" -ForegroundColor $Color_Comment
Write-Host "Press [Q] to exit..." -ForegroundColor $Color_Comment
Write-Host "$divider" -ForegroundColor $Color_Comment

while ($true) {
    if ([Console]::KeyAvailable) {
        $key = [Console]::ReadKey($true).Key
        if ($key -eq $exitKey) { break }
    }
}